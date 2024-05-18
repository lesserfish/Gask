{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.ContentEmbedding where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))

data ContentEmbedding = ContentEmbedding
    { ceValues :: [Double]
    }
    deriving (Show)

instance ToJSON ContentEmbedding where
    toJSON ce = object ["values" .= (ceValues ce)]

instance FromJSON ContentEmbedding where
    parseJSON = withObject "ContentEmbedding" $ \v ->
        ContentEmbedding <$> v .: "values"
