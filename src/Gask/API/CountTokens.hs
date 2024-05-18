{-# LANGUAGE OverloadedStrings #-}

module Gask where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:?), (.=))
import Data.Maybe (catMaybes)
import Gask.API.GenerateContent
import Gask.Types

data CountTokensRequest = CountTokensRequest
    { ctKey :: String
    , ctModel :: String
    , ctContents :: Maybe [Content]
    , ctGenerateContentRequest :: Maybe GenerateContentRequest
    , gcContents :: Maybe [Content]
    , gcSafetySettings :: Maybe [SafetySetting]
    , gcGenerationConfig :: Maybe GenerationConfig
    }
    deriving (Show)

data BodyRequest = BodyRequest
    { brContents :: Maybe [Content]
    , brGenerateContentRequest :: Maybe GenerateContentRequest
    }
    deriving (Show)

instance ToJSON BodyRequest where
    toJSON br =
        object $
            catMaybes
                [ ("contents" .=) <$> (brContents br)
                , ("generateContentRequest" .=) <$> (brGenerateContentRequest br)
                ]

instance FromJSON BodyRequest where
    parseJSON = withObject "BodyRequest" $ \v ->
        BodyRequest
            <$> v .:? "contents"
            <*> v .:? "generateContentRequest"
