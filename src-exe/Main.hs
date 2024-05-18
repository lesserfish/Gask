{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration
import Control.Monad.IO.Class
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import qualified Gask.API as G
import System.Console.ANSI
import System.Console.Haskeline
import System.Exit (ExitCode (..), exitWith)
import System.IO

defaultInput :: IO String
defaultInput =
    runInputT
        defaultSettings
        ( do
            input <- getInputLine ""
            case input of
                Nothing -> liftIO $ exitWith (ExitFailure (-1))
                Just str -> return str
        )

getStdin :: String -> IO String
getStdin buffer = do
    done <- isEOF
    if done
        then return buffer
        else do
            inp <- getLine
            getStdin (buffer ++ "\n" ++ inp)

printColored :: Color -> String -> IO ()
printColored color text = do
    setSGR [SetColor Foreground Vivid color]
    putStr text
    setSGR [Reset]

printColoredLn :: Color -> String -> IO ()
printColoredLn color text = do
    setSGR [SetColor Foreground Vivid color]
    putStrLn text
    setSGR [Reset]

extendedInput :: IO String
extendedInput = getStdin ""

chooseInput :: Bool -> IO String
chooseInput False = defaultInput
chooseInput True = defaultInput

outputEmptyResponse :: G.GenerateContentResponse -> String
outputEmptyResponse _ = "[Empty response]"

outputEmptyCandidate :: G.Candidate -> String
outputEmptyCandidate _ = "[Empty Response]"

outputPart :: G.Part -> String
outputPart (G.TextPart (G.Text text)) = text
outputPart (G.ImagePart _) = "[Image not supported]"

outputContent :: G.Content -> String
outputContent content = concat . (intersperse "\n") . (fmap outputPart) . G.cParts $ content

outputCandidate :: G.Candidate -> String
outputCandidate candidate = case (G.cContent candidate) of
    Nothing -> outputEmptyCandidate candidate
    Just content -> outputContent content

outputResponse :: G.GenerateContentResponse -> String
outputResponse response
    | (length . G.gcrCandidates $ response) == 0 = outputEmptyResponse response
    | otherwise = concat . (intersperse "\n") . (fmap outputCandidate) . G.gcrCandidates $ response

renderResponse :: (G.Result G.GenerateContentResponse) -> IO [G.Result G.GenerateContentResponse]
renderResponse (G.Fail e) = do
    let code = G.errorCode e
    let message = G.errorMessage e
    let status = G.errorStatus e
    printColoredLn Red $
        "\n\nError: "
            ++ (show code)
            ++ "\nMessage: "
            ++ (show message)
            ++ "\nStatus : "
            ++ (show status)
            ++ "\n"
    return [G.Fail e]
renderResponse (G.OK response) = do
    let output = outputResponse response
    putStr $ output
    return [G.OK response]

ask :: Args -> Configuration -> IO ()
ask args config = do
    printColored Yellow "\n : "
    message <- chooseInput (waitForEOF args) :: IO String
    let part = G.TextPart . G.Text $ message
    let content = [G.Content [part] "user"]
    let request = G.GenerateContentRequest (key config) (model config) (Just content) (safetySettings config) (generationConfig config)
    printColored Red "\n < "
    _ <- G.streamGenerateContentMC request renderResponse
    putStrLn "\n"
    return ()

getResponseContents :: G.GenerateContentResponse -> [G.Content]
getResponseContents = catMaybes . fmap G.cContent . G.gcrCandidates

getBotContents :: G.Result G.GenerateContentResponse -> Maybe [G.Content]
getBotContents (G.Fail _) = Nothing
getBotContents (G.OK r) = Just . getResponseContents $ r

chat' :: [G.Content] -> Args -> Configuration -> IO ()
chat' buffer args config = do
    printColored Yellow "\n : "
    message <- chooseInput (waitForEOF args) :: IO String
    let part = G.TextPart . G.Text $ message
    let new_content = [G.Content [part] "user"]
    let content = buffer ++ new_content
    let request = G.GenerateContentRequest (key config) (model config) (Just content) (safetySettings config) (generationConfig config)
    printColored Red "\n < "
    responses <- G.streamGenerateContentMC request renderResponse
    putStrLn "\n"
    let botContent = concat . catMaybes . (fmap getBotContents) $ responses :: [G.Content]
    chat' (content ++ botContent) args config

chat :: Args -> Configuration -> IO ()
chat = chat' []

main :: IO ()
main = do
    args <- parseArgs
    maybeConfig <- loadConfiguration args
    case maybeConfig of
        Nothing -> exitWith (ExitFailure (-1))
        Just config -> do
            if (interactive args)
                then chat args config
                else ask args config
