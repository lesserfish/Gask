{-# LANGUAGE OverloadedStrings #-}

module Gask.API.Requests where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
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

getAs :: (Aeson.FromJSON b) => String -> HTTP.Query -> IO (HTTP.Response b)
getAs path qp = do
    let uri = "generativelanguage.googleapis.com"
    let request =
            HTTP.defaultRequest
                |> (HTTP.setRequestMethod . BC.pack $ "GET")
                |> (HTTP.setRequestSecure True)
                |> (HTTP.setRequestPort 443)
                |> (HTTP.setRequestHost . BC.pack $ uri)
                |> (HTTP.setRequestPath . BC.pack $ path)
                |> (HTTP.addToRequestQueryString qp)
    HTTP.httpJSON request

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

postAs :: (Aeson.ToJSON a, Aeson.FromJSON b) => String -> HTTP.Query -> a -> IO (HTTP.Response b)
postAs path qp body = do
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
    HTTP.httpJSON request
