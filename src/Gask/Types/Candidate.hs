{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.Candidate where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:?), (.=))
import Data.Maybe (catMaybes)
import Gask.Types.CitationMetadata
import Gask.Types.Content
import Gask.Types.SafetyRating

data Candidate = Candidate
    { cContent :: Maybe Content
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
                [ ("content" .=) <$> (cContent $ candidate)
                , ("finishReason" .=) <$> (cFinishReason candidate)
                , ("safetyRatings" .=) <$> (cSafetyRatings candidate)
                , ("citationMetadata" .=) <$> (cCitationMetadata candidate)
                , ("tokenCount" .=) <$> (cTokenCount candidate)
                , ("index" .=) <$> (cIndex candidate)
                ]

instance FromJSON Candidate where
    parseJSON = withObject "Candidate" $ \v ->
        Candidate
            <$> v .:? "content"
            <*> v .:? "finishReason"
            <*> v .:? "SafetySetting"
            <*> v .:? "citationMetadata"
            <*> v .:? "tokenCount"
            <*> v .:? "index"

candidateHasText :: Candidate -> Bool
candidateHasText (Candidate Nothing _ _ _ _ _) = False
candidateHasText (Candidate (Just content) _ _ _ _ _) = contentHasText content

candidateText :: Candidate -> String
candidateText (Candidate Nothing _ _ _ _ _) = ""
candidateText (Candidate (Just content) _ _ _ _ _) = contentText content
