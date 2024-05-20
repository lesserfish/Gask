{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Gask
import qualified Gask.API as G
import Settings
import System.Console.ANSI
import qualified System.Console.Haskeline as HL
import System.Exit (ExitCode (..), exitWith)

setColor :: Color -> IO ()
setColor color = do
    setSGR [SetColor Foreground Vivid color]

resetColor :: IO ()
resetColor = do
    setSGR [Reset]

printPrompt :: Color -> String -> IO ()
printPrompt color content = do
    liftIO $ setColor $ color
    liftIO $ putStr $ content
    liftIO $ resetColor

defaultFetch :: Settings -> IO String
defaultFetch settings =
    HL.runInputT
        HL.defaultSettings
        ( do
            let quietMode = sQuietMode settings
            if quietMode
                then do
                    input <- HL.getInputLine ""
                    case input of
                        Nothing -> liftIO $ exitWith (ExitFailure (-1))
                        Just str -> do
                            return str
                else do
                    liftIO $ printPrompt (sPromptColor settings) "\n\n : "
                    liftIO $ setColor . sUserColor $ settings
                    input <- HL.getInputLine ""
                    liftIO $ resetColor
                    case input of
                        Nothing -> liftIO $ exitWith (ExitFailure (-1))
                        Just str -> do
                            liftIO $ printPrompt (sPromptColor settings) "\n\n > "
                            return str
        )

eofFetch :: Settings -> IO String
eofFetch settings = do
    let quietMode = sQuietMode settings
    if quietMode
        then do
            HL.runInputT HL.defaultSettings (loop "")
        else do
            liftIO $ printPrompt (sPromptColor settings) "\n\n : "
            liftIO $ setColor . sUserColor $ settings
            str <- HL.runInputT HL.defaultSettings (loop "")
            liftIO $ printPrompt (sPromptColor settings) "\n\n > "
            return str
  where
    loop buffer = do
        input <- HL.getInputLine ""
        case input of
            Nothing -> return buffer
            Just str -> do
                loop (buffer ++ "\n" ++ str)

renderError :: G.Error -> IO ()
renderError e = do
    let code = G.errorCode e
    let message = G.errorMessage e
    let status = G.errorStatus e
    setColor Red
    putStrLn $
        "\n\nError: "
            ++ (show code)
            ++ "\nMessage: "
            ++ (show message)
            ++ "\nStatus : "
            ++ (show status)
            ++ "\n"
    resetColor

defaultRender :: Settings -> (G.Result G.GenerateContentResponse) -> IO [G.GenerateContentResponse]
defaultRender settings (G.Fail e) = do
    let quietMode = sQuietMode settings
    if quietMode
        then return []
        else do
            renderError e
            return []
defaultRender settings (G.OK response) = do
    let quietMode = sQuietMode settings
    if G.responseHasText response
        then do
            if quietMode
                then do
                    putStr $ G.responseText response
                    return [response]
                else do
                    setColor . sModelColor $ settings
                    putStr $ G.responseText response
                    resetColor
                    return [response]
        else do
            if quietMode
                then return []
                else do
                    putStr $ "[No response]"
                    return []

getFetchFunction :: Settings -> (Gask -> IO String)
getFetchFunction settings
    | sWaitForEOF settings = \_ -> eofFetch settings
    | otherwise = \_ -> defaultFetch settings

getRender :: Settings -> (G.Result G.GenerateContentResponse) -> IO [G.GenerateContentResponse]
getRender settings
    | sInteractive settings = defaultRender settings
    | otherwise =
        ( \i -> do
            _ <- (defaultRender settings) i
            return []
        )

runChat :: Settings -> IO Gask
runChat settings
    | (sInput settings) == "" =
        chatMethod
            (not . sInteractive $ settings)
            settings
            (getFetchFunction settings)
            (getRender settings)
    | otherwise = do
        _ <- if (sQuietMode settings) then return $ () else printPrompt (sPromptColor settings) " > "
        chatFromStringMethod
            (sInput settings)
            (not . sInteractive $ settings)
            settings
            (getFetchFunction settings)
            (getRender settings)
  where
    chatMethod = if (sPrompt settings == "") then chat else resumeChat history
    chatFromStringMethod = if (sPrompt settings == "") then chatFromString else resumeChatFromString history
    history = [G.newUserText . sPrompt $ settings]

main :: IO ()
main = do
    maybeSettings <- loadSettings
    case maybeSettings of
        Nothing -> exitWith (ExitFailure (-1))
        Just settings -> do
            _ <- runChat settings
            return ()
