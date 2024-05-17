{-# LANGUAGE OverloadedStrings #-}

module Gask.API.List where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import Data.Maybe (catMaybes)
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

data ModelList = ModelList
    { mlModels :: [ModelInfo]
    , mlNextPageToken :: Maybe String
    }
    deriving (Show)

instance ToJSON ModelList where
    toJSON ml =
        object $
            catMaybes
                [ ("models" .=) <$> (Just $ mlModels ml)
                , ("nextPageToken" .=) <$> (mlNextPageToken ml)
                ]

instance FromJSON ModelList where
    parseJSON = withObject "ModelList" $ \v ->
        ModelList
            <$> v .: "models"
            <*> v .:? "nextPageToken"

path :: String
path = "/v1/models"

getModelList :: ListRequest -> IO (Either ModelList Error)
getModelList (ListRequest a b c) = do
    let qp = QueryParameters a b c
    response <- get path (query qp)
    let statuscode = getResponseStatusCode response
    let json = getResponseBody response
    if statuscode /= 200
        then return $ Right (Error statuscode (Just "") (Just ""))
        else return $ tryParse json
