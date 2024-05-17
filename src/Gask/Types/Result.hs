{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gask.Types.Result where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, parseJSON, toJSON)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import Gask.Types.Error

data Result a = OK a | Fail Error

instance (ToJSON a) => ToJSON (Result a) where
    toJSON (OK a) = toJSON a
    toJSON (Fail e) = toJSON e

instance (FromJSON a) => FromJSON (Result a) where
    parseJSON value =
        fmap OK (parseJSON value :: Parser a)
            <|> fmap Fail (parseJSON value :: Parser Error)

instance (Show a) => Show (Result a) where
    show (OK c) = show c
    show (Fail e) = show e

tryError :: BS.ByteString -> String -> Error
tryError json error_message = case (eitherDecodeStrict json) of
    Left _ -> Error (-1) (Just error_message) (Just "")
    Right e -> e

tryParse :: (FromJSON a) => BS.ByteString -> Result a
tryParse json = case (eitherDecodeStrict json) of
    Left str -> Fail $ tryError json str
    Right obj -> OK obj
