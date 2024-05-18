{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.Error (Error (..)) where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import Data.Maybe (catMaybes)

data Error = Error
    { errorCode :: Int
    , errorMessage :: Maybe String
    , errorStatus :: Maybe String
    }
    deriving (Show)

instance ToJSON Error where
    toJSON err =
        object $
            [ "error"
                .= object
                    ( catMaybes
                        [ Just ("code" .= errorCode err)
                        , ("message" .=) <$> (errorMessage err)
                        , ("status" .=) <$> (errorStatus err)
                        ]
                    )
            ]

instance FromJSON Error where
    parseJSON = withObject "Error" $ \v -> do
        errorobj <- v .: "error"
        code <- errorobj .: "code"
        msg <- errorobj .:? "message"
        status <- errorobj .:? "status"
        return (Error code msg status)
