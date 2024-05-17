{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.CitationMetadata where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import Data.Maybe (catMaybes)

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
