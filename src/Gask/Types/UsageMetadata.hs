{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.UsageMetadata where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))

data UsageMetadata = UsageMetadata
    { umdPromptTokenCount :: Int
    , umdCandidatesTokenCount :: Int
    , umdTotalTokenCount :: Int
    }
    deriving (Show)

instance ToJSON UsageMetadata where
    toJSON metadata =
        object $
            [ "promptTokenCount" .= (umdPromptTokenCount metadata)
            , "candidatesTokenCount" .= (umdCandidatesTokenCount metadata)
            , "totalTokenCount" .= (umdTotalTokenCount metadata)
            ]

instance FromJSON UsageMetadata where
    parseJSON = withObject "UsageMetadata" $ \v ->
        UsageMetadata
            <$> v .: "promptTokenCount"
            <*> v .: "candidatesTokenCount"
            <*> v .: "totalTokenCount"
