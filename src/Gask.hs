{-# LANGUAGE OverloadedStrings #-}

module Gask where

import Conduit
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.ByteString
import Data.JsonStream.Conduit
import Data.JsonStream.Parser (Parser, arrayOf, integer)
import Gask.API.GenerateContent
import Gask.Types

defaultPart :: String -> Part
defaultPart str = TextPart (Text str)

defaultContent :: String -> Content
defaultContent str = Content [defaultPart str] "user"

defaultRequest :: String -> GenerateContentRequest
defaultRequest str = GenerateContentRequest "AIzaSyABGXkzWRrhN6TAcUS9IXG8HlTbC6XUAoU" "models/gemini-pro" (Just [defaultContent str]) Nothing Nothing

lineSource :: (MonadIO m) => ConduitT () ByteString m ()
lineSource = do
    forM_ ["[1, 2, 3", "4, 5, 6,", "7, 8, 9]"] $ \i -> do
        yield $ i
        liftIO $ threadDelay 1000000 -- Delay for 1 second (1,000,000 microseconds)

process :: ConduitM Int Void IO ()
process = do
    mapM_C (putStrLn . show)

myparser :: Parser Int
myparser = arrayOf integer

myConduit :: ConduitM ByteString Int IO ()
myConduit = void $ (parserConduit myparser)

test :: ConduitT () Void IO ()
test = lineSource .| myConduit .| process
