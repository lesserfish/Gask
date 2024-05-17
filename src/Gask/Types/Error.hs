{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.Error (Error (..), tryParse) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString as BS
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

tryError :: BS.ByteString -> String -> Error
tryError json error_message = case (eitherDecodeStrict json) of
    Left _ -> Error (-1) (Just error_message) (Just "")
    Right e -> e

tryParse :: (FromJSON a) => BS.ByteString -> Either a Error
tryParse json = case (eitherDecodeStrict json) of
    Left str -> Right $ tryError json str
    Right obj -> Left obj
