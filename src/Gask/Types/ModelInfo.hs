{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.ModelInfo (ModelInfo (..)) where

import Data.Aeson
import Data.Maybe (catMaybes)

data ModelInfo = ModelInfo
    { modelName :: Maybe String
    , modelBaseModelId :: Maybe String
    , modelVersion :: Maybe String
    , modelDisplayName :: Maybe String
    , modelDescription :: Maybe String
    , modelInputTokenLimit :: Maybe Int
    , modelOutputTokenLimit :: Maybe Int
    , modelSupportedGenerationMethods :: Maybe [String]
    , modelTemperature :: Maybe Double
    , modelTopP :: Maybe Double
    , modelTopK :: Maybe Int
    }
    deriving (Show)

instance ToJSON ModelInfo where
    toJSON m =
        object $
            catMaybes
                [ ("name" .=) <$> (modelName m)
                , ("baseModelId" .=) <$> (modelBaseModelId m)
                , ("version" .=) <$> (modelVersion m)
                , ("displayName" .=) <$> (modelDisplayName m)
                , ("description" .=) <$> (modelDescription m)
                , ("inputTokenLimit" .=) <$> (modelInputTokenLimit m)
                , ("outputTokenLimit" .=) <$> (modelOutputTokenLimit m)
                , ("supportedGenerationMethods" .=) <$> (modelSupportedGenerationMethods m)
                , ("temperature" .=) <$> (modelTemperature m)
                , ("topP" .=) <$> (modelTopP m)
                , ("topK" .=) <$> (modelTopK m)
                ]

instance FromJSON ModelInfo where
    parseJSON = withObject "ModelInfo" $ \v ->
        ModelInfo
            <$> v .:? "name"
            <*> v .:? "baseModelId"
            <*> v .:? "version"
            <*> v .:? "displayName"
            <*> v .:? "description"
            <*> v .:? "inputTokenLimit"
            <*> v .:? "outputTokenLimit"
            <*> v .:? "supportedGenerationMethods"
            <*> v .:? "temperature"
            <*> v .:? "topP"
            <*> v .:? "topK"
