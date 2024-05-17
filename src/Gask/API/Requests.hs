{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gask.API.Requests where

import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import Data.JsonStream.Conduit
import Data.JsonStream.Parser (Parser, arrayOf, value)
import qualified Network.HTTP.Simple as HTTP

class Queryable a where
    query :: a -> [(BS.ByteString, Maybe BS.ByteString)]

queryHelper :: String -> Maybe String -> [(BS.ByteString, Maybe BS.ByteString)]
queryHelper _ Nothing = []
queryHelper k (Just v) = [(key, val)]
  where
    key = BC.pack $ k
    val = Just . BC.pack $ v

(|>) :: a -> (a -> b) -> b
(|>) a f = f $ a

get :: String -> HTTP.Query -> IO (HTTP.Response BS.ByteString)
get path qp = do
    let uri = "generativelanguage.googleapis.com"
    let request =
            HTTP.defaultRequest
                |> (HTTP.setRequestMethod . BC.pack $ "GET")
                |> (HTTP.setRequestSecure True)
                |> (HTTP.setRequestPort 443)
                |> (HTTP.setRequestHost . BC.pack $ uri)
                |> (HTTP.setRequestPath . BC.pack $ path)
                |> (HTTP.addToRequestQueryString qp)
    HTTP.httpBS request

post :: (Aeson.ToJSON a) => String -> HTTP.Query -> a -> IO (HTTP.Response BS.ByteString)
post path qp body = do
    let uri = "generativelanguage.googleapis.com"
    let request =
            HTTP.defaultRequest
                |> (HTTP.setRequestMethod . BC.pack $ "POST")
                |> (HTTP.setRequestSecure True)
                |> (HTTP.setRequestPort 443)
                |> (HTTP.setRequestHost . BC.pack $ uri)
                |> (HTTP.setRequestPath . BC.pack $ path)
                |> (HTTP.setRequestBodyJSON $ body)
                |> (HTTP.addToRequestQueryString qp)
    HTTP.httpBS request

postStream :: forall a b c. (Aeson.ToJSON a, Aeson.FromJSON b) => String -> HTTP.Query -> a -> (ConduitT b Void IO c) -> IO c
postStream path qp body sink = do
    let parser = arrayOf (value :: Parser b)
    let combinator = void $ parserConduit parser :: ConduitT BS.ByteString b IO ()
    let uri = "generativelanguage.googleapis.com"
    let request =
            HTTP.defaultRequest
                |> (HTTP.setRequestMethod . BC.pack $ "POST")
                |> (HTTP.setRequestSecure True)
                |> (HTTP.setRequestPort 443)
                |> (HTTP.setRequestHost . BC.pack $ uri)
                |> (HTTP.setRequestPath . BC.pack $ path)
                |> (HTTP.setRequestBodyJSON $ body)
                |> (HTTP.addToRequestQueryString qp)
    let conduit = combinator .| sink :: ConduitT BS.ByteString Void IO c
    HTTP.httpSink request (\r -> conduit)
