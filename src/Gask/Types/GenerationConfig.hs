{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.GenerationConfig where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Maybe (catMaybes)

data GenerationConfig = GenerationConfig
    { gcStopSequences :: Maybe [String]
    , gcCandidateCount :: Maybe Int
    , gcMaxOutputTokens :: Maybe Int
    , gcTemperature :: Maybe Double
    , gcTopP :: Maybe Double
    , gcTopK :: Maybe Int
    }
    deriving (Show)

instance ToJSON GenerationConfig where
    toJSON config =
        object $
            catMaybes
                [ ("stopSequences" .=) <$> (gcStopSequences config)
                , ("candidateCount" .=) <$> (gcCandidateCount config)
                , ("maxOutputTokens" .=) <$> (gcMaxOutputTokens config)
                , ("temperature" .=) <$> (gcTemperature config)
                , ("topP" .=) <$> (gcTopP config)
                , ("topK" .=) <$> (gcTopK config)
                ]

instance FromJSON GenerationConfig where
    parseJSON = withObject "GenerationConfig" $ \v ->
        GenerationConfig
            <$> v .: "stopSequences"
            <*> v .: "candidateCount"
            <*> v .: "maxOutputTokens"
            <*> v .: "temperature"
            <*> v .: "topP"
            <*> v .: "topK"
