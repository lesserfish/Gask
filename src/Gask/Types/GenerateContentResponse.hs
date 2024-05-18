{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.GenerateContentResponse where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import Data.Maybe (catMaybes)
import Gask.Types.Candidate
import Gask.Types.Content
import Gask.Types.PromptFeedback
import Gask.Types.UsageMetadata

data GenerateContentResponse = GenerateContentResponse
    { gcrCandidates :: [Candidate]
    , gcrPromptFeedback :: Maybe PromptFeedback
    , gcrUsageMetadata :: Maybe UsageMetadata
    }
    deriving (Show)

instance ToJSON GenerateContentResponse where
    toJSON response =
        object $
            catMaybes
                [ ("candidates" .=) <$> (Just . gcrCandidates $ response)
                , ("promptFeedback" .=) <$> (gcrPromptFeedback $ response)
                , ("usageMetadata" .=) <$> (gcrUsageMetadata response)
                ]

instance FromJSON GenerateContentResponse where
    parseJSON = withObject "GenerateContentResponse" $ \v ->
        GenerateContentResponse
            <$> v .: "candidates"
            <*> v .:? "promptFeedback"
            <*> v .:? "usageMetadata"

responseHasText :: GenerateContentResponse -> Bool
responseHasText (GenerateContentResponse [] _ _) = False
responseHasText (GenerateContentResponse candidates _ _) = (foldl (||) False) . (fmap candidateHasText) $ candidates

responseText :: GenerateContentResponse -> String
responseText (GenerateContentResponse candidates _ _) = concat . (fmap candidateText) $ candidates

responseContents :: GenerateContentResponse -> [Content]
responseContents = catMaybes . fmap cContent . gcrCandidates
