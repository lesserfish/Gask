{-# LANGUAGE OverloadedStrings #-}

module Gask.API.Get (GetRequest (..), getModelInfo) where

import Gask.API.Requests
import Gask.Types
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode)

data QueryParameters = QueryParameters
    { qpKey :: String
    }
    deriving (Show)

data GetRequest = GetRequest
    { gKey :: String
    , gModelName :: String
    }
    deriving (Show)

instance Queryable QueryParameters where
    query (QueryParameters k) =
        queryHelper "key" (Just k)

path :: String -> String
path model_name = "/v1/" ++ model_name

getModelInfo :: GetRequest -> IO (Result ModelInfo)
getModelInfo (GetRequest a mn) = do
    let qp = QueryParameters a
    response <- get (path mn) (query qp)
    let statuscode = getResponseStatusCode response
    let json = getResponseBody response
    if statuscode /= 200
        then return $ Fail $ (Error statuscode (Just "") (Just ""))
        else return $ tryParse json
