{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.SafetyRating where

import Data.Aeson (FromJSON, ToJSON, Value (..), object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import Data.Maybe (catMaybes)
import Gask.Types.SafetySetting

data FinishReason
    = FR_FINISH_REASON_UNSPECIFIED
    | FR_STOP
    | FR_MAX_TOKENS
    | FR_SAFETY
    | FR_RECITATION
    | FR_OTHER
    deriving (Show)

instance ToJSON FinishReason where
    toJSON FR_FINISH_REASON_UNSPECIFIED = String "FINISH_REASON_UNSPECIFIED"
    toJSON FR_STOP = String "STOP"
    toJSON FR_MAX_TOKENS = String "MAX_TOKENS"
    toJSON FR_SAFETY = String "SAFETY"
    toJSON FR_RECITATION = String "RECITATION"
    toJSON FR_OTHER = String "OTHER"

instance FromJSON FinishReason where
    parseJSON (String "FINISH_REASON_UNSPECIFIED") = pure FR_FINISH_REASON_UNSPECIFIED
    parseJSON (String "STOP") = pure FR_STOP
    parseJSON (String "MAX_TOKENS") = pure FR_MAX_TOKENS
    parseJSON (String "SAFETY") = pure FR_SAFETY
    parseJSON (String "RECITATION") = pure FR_RECITATION
    parseJSON (String "OTHER") = pure FR_OTHER
    parseJSON _ = fail "Invalid FinishReason"

data HarmProbability
    = HP_HARM_PROBABILITY_UNSPECIFIED
    | HP_NEGLIGIBLE
    | HP_LOW
    | HP_MEDIUM
    | HP_HIGH
    deriving (Show)

instance ToJSON HarmProbability where
    toJSON HP_HARM_PROBABILITY_UNSPECIFIED = String "HARM_PROBABILITY_UNSPECIFIED"
    toJSON HP_NEGLIGIBLE = String "NEGLIGIBLE"
    toJSON HP_LOW = String "LOW"
    toJSON HP_MEDIUM = String "MEDIUM"
    toJSON HP_HIGH = String "HIGH"

instance FromJSON HarmProbability where
    parseJSON (String "HARM_PROBABILITY_UNSPECIFIED") = pure HP_HARM_PROBABILITY_UNSPECIFIED
    parseJSON (String "NEGLIGIBLE") = pure HP_NEGLIGIBLE
    parseJSON (String "LOW") = pure HP_LOW
    parseJSON (String "MEDIUM") = pure HP_MEDIUM
    parseJSON (String "HIGH") = pure HP_HIGH
    parseJSON _ = fail "Invalid HarmProbability"

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
