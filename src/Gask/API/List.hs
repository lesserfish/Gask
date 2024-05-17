{-# LANGUAGE OverloadedStrings #-}

module Gask.API.List (ListRequest (..), getModelList) where

import Gask.API.Requests
import Gask.Types
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode)

data QueryParameters = QueryParameters
    { qKey :: String
    , qPageSize :: Maybe Int
    , qPageToken :: Maybe String
    }
    deriving (Show)

instance Queryable QueryParameters where
    query (QueryParameters k ps pt) =
        queryHelper "key" (Just k)
            ++ queryHelper "pageSize" (fmap show ps)
            ++ queryHelper "pageToken" (fmap show pt)

data ListRequest = ListRequest
    { lKey :: String
    , lPageSize :: Maybe Int
    , lPageToken :: Maybe String
    }
    deriving (Show)

path :: String
path = "/v1/models"

getModelList :: ListRequest -> IO (Result ModelList)
getModelList (ListRequest a b c) = do
    let qp = QueryParameters a b c
    response <- get path (query qp)
    let statuscode = getResponseStatusCode response
    let json = getResponseBody response
    if statuscode /= 200
        then return $ Fail (Error statuscode (Just "") (Just ""))
        else return $ tryParse json
