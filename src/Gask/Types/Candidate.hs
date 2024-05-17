{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.Candidate where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import Data.Maybe (catMaybes)
import Gask.Types.CitationMetadata
import Gask.Types.Content
import Gask.Types.SafetyRating

data Candidate = Candidate
    { cContent :: Content
    , cFinishReason :: Maybe FinishReason
    , cSafetyRatings :: Maybe [SafetyRating]
    , cCitationMetadata :: Maybe CitationMetadata
    , cTokenCount :: Maybe Int
    , cIndex :: Maybe Int
    }
    deriving (Show)

instance ToJSON Candidate where
    toJSON candidate =
        object $
            catMaybes
                [ ("content" .=) <$> (Just . cContent $ candidate)
                , ("finishReason" .=) <$> (cFinishReason candidate)
                , ("safetyRatings" .=) <$> (cSafetyRatings candidate)
                , ("citationMetadata" .=) <$> (cCitationMetadata candidate)
                , ("tokenCount" .=) <$> (cTokenCount candidate)
                , ("index" .=) <$> (cIndex candidate)
                ]

instance FromJSON Candidate where
    parseJSON = withObject "Candidate" $ \v ->
        Candidate
            <$> v .: "content"
            <*> v .:? "finishReason"
            <*> v .:? "SafetySetting"
            <*> v .:? "citationMetadata"
            <*> v .:? "tokenCount"
            <*> v .:? "index"
