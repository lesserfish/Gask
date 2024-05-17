{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.GenerateContentResponse where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import Data.Maybe (catMaybes)
import GHC.Generics
import Gask.Types.Content
import Gask.Types.SafetySetting

data FinishReason
    = FINISH_REASON_UNSPECIFIED
    | STOP
    | MAX_TOKENS
    | SAFETY
    | RECITATION
    | OTHER
    deriving (Show, Generic)

instance FromJSON FinishReason
instance ToJSON FinishReason

data HarmProbability
    = HARM_PROBABILITY_UNSPECIFIED
    | NEGLIGIBLE
    | LOW
    | MEDIUM
    | HIGH
    deriving (Show, Generic)

instance FromJSON HarmProbability
instance ToJSON HarmProbability

data SafetyRating = SafetyRating
    { srCategory :: HarmCategory
    , srProbability :: HarmProbability
    , srBlock :: Maybe Bool
    }
    deriving (Show)

instance ToJSON SafetyRating where
    toJSON rating =
        object $
            catMaybes
                [ ("category" .=) <$> (Just . srCategory $ rating)
                , ("probability" .=) <$> (Just . srProbability $ rating)
                , ("blocked" .=) <$> (srBlock rating)
                ]

instance FromJSON SafetyRating where
    parseJSON = withObject "SafetyRating" $ \v ->
        SafetyRating
            <$> v .: "category"
            <*> v .: "probability"
            <*> v .:? "blocked"

data CitationSource = CitationSource
    { csStartIndex :: Maybe Int
    , csEndIndex :: Maybe Int
    , csURI :: Maybe String
    , csLicense :: Maybe String
    }
    deriving (Show)

instance ToJSON CitationSource where
    toJSON source =
        object $
            catMaybes
                [ ("startIndex" .=) <$> (csStartIndex source)
                , ("endIndex" .=) <$> (csEndIndex source)
                , ("uri" .=) <$> (csURI source)
                , ("license" .=) <$> (csLicense source)
                ]

instance FromJSON CitationSource where
    parseJSON = withObject "CitationSource" $ \v ->
        CitationSource
            <$> v .:? "startIndex"
            <*> v .:? "endIndex"
            <*> v .:? "uri"
            <*> v .:? "license"

data CitationMetadata = CitationMetadata
    { cmdCitationSources :: [CitationSource]
    }
    deriving (Show)

instance ToJSON CitationMetadata where
    toJSON metadata =
        object $ ["citationSources" .= (cmdCitationSources metadata)]

instance FromJSON CitationMetadata where
    parseJSON = withObject "CitationMetadata" $ \v -> CitationMetadata <$> v .: "citationSources"

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
