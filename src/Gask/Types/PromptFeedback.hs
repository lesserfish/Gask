{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.PromptFeedback where

import Data.Aeson (FromJSON, ToJSON, Value (..), object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Maybe (catMaybes)
import Gask.Types.SafetyRating

data BlockReason
    = BR_BLOCK_REASON_UNSPECIFIED
    | BR_SAFETY
    | BR_OTHER
    deriving (Show)

instance ToJSON BlockReason where
    toJSON BR_BLOCK_REASON_UNSPECIFIED = String "BLOCK_REASON_UNSPECIFIED"
    toJSON BR_SAFETY = String "SAFETY"
    toJSON BR_OTHER = String "OTHER"

instance FromJSON BlockReason where
    parseJSON (String "BLOCK_REASON_UNSPECIFIED") = pure BR_BLOCK_REASON_UNSPECIFIED
    parseJSON (String "SAFETY") = pure BR_SAFETY
    parseJSON (String "OTHER") = pure BR_OTHER
    parseJSON _ = fail "Invalid BlockReason"

data PromptFeedback = PromptFeedback
    { pfBlockReason :: Maybe BlockReason
    , pfSafetyRatings :: Maybe [SafetyRating]
    }
    deriving (Show)

instance ToJSON PromptFeedback where
    toJSON feedback =
        object $
            catMaybes
                [ ("blockReason" .=) <$> (pfBlockReason feedback)
                , ("safetyRatings" .=) <$> (pfSafetyRatings feedback)
                ]

instance FromJSON PromptFeedback where
    parseJSON = withObject "PromptFeedback" $ \v ->
        PromptFeedback
            <$> v .: "blockReason"
            <*> v .: "safetyRatings"
