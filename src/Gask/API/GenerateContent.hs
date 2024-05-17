{-# LANGUAGE OverloadedStrings #-}

module Gask.API.GenerateContent where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:?), (.=))
import Data.Maybe (catMaybes)
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

data GenerateContentRequest = GenerateContentRequest
    { gcKey :: String
    , gcModel :: String
    , gcContents :: Maybe [Content]
    , gcSafetySettings :: Maybe [SafetySetting]
    , gcGenerationConfig :: Maybe GenerationConfig
    }
    deriving (Show)

path :: String -> String
path model_name = "/v1/" ++ model_name ++ ":generateContent"

generateContent :: GenerateContentRequest -> IO ()
generateContent (GenerateContentRequest key model contents ss gc) = do
    let qp = QueryParameters key
    let br = BodyRequest contents ss gc
    response <- post (path model) (query qp) br
    putStrLn $ show response
