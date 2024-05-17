{-# LANGUAGE OverloadedStrings #-}

module Gask.API.GenerateContent (GenerateContentRequest (..), generateContent) where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:?), (.=))
import Data.Maybe (catMaybes)
import Gask.API.Requests
import Gask.Types
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode)

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

generateContent :: GenerateContentRequest -> IO (Either GenerateContentResponse Error)
generateContent (GenerateContentRequest key model contents ss gc) = do
    let qp = QueryParameters key
    let br = BodyRequest contents ss gc
    response <- post (path model) (query qp) br
    let statuscode = getResponseStatusCode response
    let json = getResponseBody response
    if statuscode /= 200
        then return $ Right (Error statuscode (Just "") (Just ""))
        else return $ tryParse json
