{-# LANGUAGE OverloadedStrings #-}

module Gask.API.StreamGenerateContent (StreamGenerateContentRequest, streamGenerateContent, streamGenerateContentC) where

import Conduit (ConduitT, mapM_C)
import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:?), (.=))
import Data.Maybe (catMaybes)
import Data.Void
import Gask.API.GenerateContent (GenerateContentRequest (..))
import Gask.API.Requests
import Gask.Types

data QueryParameters = QueryParameters
    { qKey :: String
    }
    deriving (Show)

instance Queryable QueryParameters where
    query (QueryParameters k) =
        queryHelper "key" (Just k)

data BodyRequest = BodyRequest
    { brContents :: Maybe [Content]
    , brSafetySettings :: Maybe [SafetySetting]
    , brGenerationConfig :: Maybe GenerationConfig
    }
    deriving (Show)

instance ToJSON BodyRequest where
    toJSON br =
        object $
            catMaybes
                [ ("contents" .=) <$> (brContents br)
                , ("safetySettings" .=) <$> (brSafetySettings br)
                , ("generationConfig" .=) <$> (brGenerationConfig br)
                ]

instance FromJSON BodyRequest where
    parseJSON = withObject "BodyRequest" $ \v ->
        BodyRequest
            <$> v .:? "contents"
            <*> v .:? "safetySettings"
            <*> v .:? "generationConfig"

type StreamGenerateContentRequest = GenerateContentRequest

path :: String -> String
path model_name = "/v1/" ++ model_name ++ ":streamGenerateContent"

toConduit :: (GenerateContentResponse -> IO ()) -> (ConduitT GenerateContentResponse Void IO ())
toConduit f = mapM_C f

streamGenerateContent :: StreamGenerateContentRequest -> (GenerateContentResponse -> IO ()) -> IO ()
streamGenerateContent (GenerateContentRequest key model contents ss gc) handler = do
    let qp = QueryParameters key
    let br = BodyRequest contents ss gc
    postStream (path model) (query qp) br (toConduit handler)
    return ()

streamGenerateContentC :: StreamGenerateContentRequest -> (ConduitT GenerateContentResponse Void IO a) -> IO a
streamGenerateContentC (GenerateContentRequest key model contents ss gc) sink = do
    let qp = QueryParameters key
    let br = BodyRequest contents ss gc
    postStream (path model) (query qp) br sink
