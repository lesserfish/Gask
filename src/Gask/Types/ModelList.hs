{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.ModelList where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import Data.Maybe (catMaybes)
import Gask.Types.ModelInfo

data ModelList = ModelList
    { mlModels :: [ModelInfo]
    , mlNextPageToken :: Maybe String
    }
    deriving (Show)

instance ToJSON ModelList where
    toJSON ml =
        object $
            catMaybes
                [ ("models" .=) <$> (Just $ mlModels ml)
                , ("nextPageToken" .=) <$> (mlNextPageToken ml)
                ]

instance FromJSON ModelList where
    parseJSON = withObject "ModelList" $ \v ->
        ModelList
            <$> v .: "models"
            <*> v .:? "nextPageToken"
