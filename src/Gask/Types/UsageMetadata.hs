{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.UsageMetadata where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:?), (.=))
import Data.Maybe (catMaybes)

data UsageMetadata = UsageMetadata
    { umdPromptTokenCount :: Maybe Int
    , umdCandidatesTokenCount :: Maybe Int
    , umdTotalTokenCount :: Maybe Int
    }
    deriving (Show)

instance ToJSON UsageMetadata where
    toJSON metadata =
        object $
            catMaybes
                [ ("promptTokenCount" .=) <$> (umdPromptTokenCount metadata)
                , ("candidatesTokenCount" .=) <$> (umdCandidatesTokenCount metadata)
                , ("totalTokenCount" .=) <$> (umdTotalTokenCount metadata)
                ]

instance FromJSON UsageMetadata where
    parseJSON = withObject "UsageMetadata" $ \v ->
        UsageMetadata
            <$> v .:? "promptTokenCount"
            <*> v .:? "candidatesTokenCount"
            <*> v .:? "totalTokenCount"
