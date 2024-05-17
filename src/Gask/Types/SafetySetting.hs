{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.SafetySetting where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import GHC.Generics

data HarmCategory
    = HARM_CATEGORY_UNSPECIFIED
    | HARM_CATEGORY_DEROGATORY
    | HARM_CATEGORY_TOXICITY
    | HARM_CATEGORY_VIOLENCE
    | HARM_CATEGORY_SEXUAL
    | HARM_CATEGORY_MEDICAL
    | HARM_CATEGORY_DANGEROUS
    | HARM_CATEGORY_HARASSMENT
    | HARM_CATEGORY_HATE_SPEECH
    | HARM_CATEGORY_SEXUALLY_EXPLICIT
    | HARM_CATEGORY_DANGEROUS_CONTENT
    deriving (Show, Generic)

instance FromJSON HarmCategory
instance ToJSON HarmCategory

data HarmBlockThreshold
    = HARM_BLOCK_THRESHOLD_UNSPECIFIED
    | BLOCK_LOW_AND_ABOVE
    | BLOCK_MEDIUM_AND_ABOVE
    | BLOCK_ONLY_HIGH
    | BLOCK_NONE
    deriving (Show, Generic)

instance FromJSON HarmBlockThreshold
instance ToJSON HarmBlockThreshold

data SafetySetting = SafetySetting
    { ssCategory :: HarmCategory
    , ssThreshold :: HarmBlockThreshold
    }
    deriving (Show)

instance ToJSON SafetySetting where
    toJSON safetySetting =
        object $
            [ "category" .= (ssCategory safetySetting)
            , "threshold" .= (ssThreshold safetySetting)
            ]

instance FromJSON SafetySetting where
    parseJSON = withObject "SafetySetting" $ \v ->
        SafetySetting
            <$> v .: "category"
            <*> v .: "threshold"
