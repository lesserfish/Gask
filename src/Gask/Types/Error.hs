{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.Error (Error (..), tryParse) where

import Data.Aeson (FromJSON, ToJSON, decodeStrict, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
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

helper :: (FromJSON a, FromJSON b) => BS.ByteString -> b -> Either a b
helper json generic_other = case (decodeStrict json) of
    Nothing -> Right other
    Just obj -> Left obj
  where
    maybeother = decodeStrict json
    other = case maybeother of
        Nothing -> generic_other
        Just o -> o

tryParse :: (FromJSON a) => BS.ByteString -> Either a Error
tryParse json = helper json err
  where
    err = Error (-123) (Just . show $ json) (Just "") :: Error
