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
                    liftIO $ setColor . sPromptColor $ settings
                    liftIO $ putStr "\n\n : "
                    liftIO $ setColor . sUserColor $ settings
                    input <- HL.getInputLine ""
                    liftIO $ resetColor
                    case input of
                        Nothing -> liftIO $ exitWith (ExitFailure (-1))
                        Just str -> do
                            liftIO $ setColor . sPromptColor $ settings
                            liftIO $ putStr $ "\n\n > "
                            liftIO $ resetColor
                            return str
        )

eofFetch :: Settings -> IO String
eofFetch settings = do
    let quietMode = sQuietMode settings
    if quietMode
        then do
            HL.runInputT HL.defaultSettings (loop "")
        else do
            liftIO $ setColor . sPromptColor $ settings
            liftIO $ putStr "\n\n : "
            liftIO $ setColor . sUserColor $ settings
            str <- HL.runInputT HL.defaultSettings (loop "")
            liftIO $ setColor . sPromptColor $ settings
            liftIO $ putStr $ "\n\n > "
            liftIO $ resetColor
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

main :: IO ()
main = do
    maybeSettings <- loadSettings
    case maybeSettings of
        Nothing -> exitWith (ExitFailure (-1))
        Just settings -> do
            if (sInteractive settings)
                then do
                    _ <-
                        chat
                            False
                            settings
                            (getFetchFunction settings)
                            (getRender settings)
                    return ()
                else do
                    _ <-
                        chat
                            True
                            settings
                            (getFetchFunction settings)
                            (getRender settings)
                    return ()
