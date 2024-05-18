{-# LANGUAGE OverloadedStrings #-}

module Gask.API.BatchEmbedContents (BatchEmbedContentsRequest (..), EmbedContentRequestInstance (..), batchEmbedContents) where

import Data.Aeson (FromJSON, ToJSON, encode, object, parseJSON, toJSON, withObject, (.:), (.=))
import Gask.API.EmbedContent
import Gask.API.Requests
import Gask.Types
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode)

data QueryParameters = QueryParameters
    { qpKey :: String
    }
    deriving (Show)

instance Queryable QueryParameters where
    query (QueryParameters k) =
        queryHelper "key" (Just k)

data BatchEmbedContentsRequest = BatchEmbedContentsRequest
    { becKey :: String
    , becModelName :: String
    , becRequests :: [EmbedContentRequestInstance]
    }
    deriving (Show)

data BodyRequest = BodyRequest
    { brRequests :: [EmbedContentRequestInstance]
    }
    deriving (Show)

instance ToJSON BodyRequest where
    toJSON br = object $ ["requests" .= (brRequests br)]

instance FromJSON BodyRequest where
    parseJSON = withObject "BodyRequest" $ \v -> BodyRequest <$> v .: "requests"

data CEHelper = CEHelper
    { cehEmbedding :: [ContentEmbedding]
    }
    deriving (Show)

instance ToJSON CEHelper where
    toJSON ceh = object $ ["embeddings" .= (cehEmbedding ceh)]

instance FromJSON CEHelper where
    parseJSON = withObject "CEHelper" $ \v -> CEHelper <$> v .: "embeddings"

path :: String -> String
path model_name = "/v1/" ++ model_name ++ ":batchEmbedContents"

batchEmbedContents :: BatchEmbedContentsRequest -> IO (Result [ContentEmbedding])
batchEmbedContents (BatchEmbedContentsRequest k mn r) = do
    let qp = QueryParameters k
    let br = BodyRequest r
    putStrLn . show . encode $ br
    response <- post (path mn) (query qp) br
    let statuscode = getResponseStatusCode response
    let json = getResponseBody response
    if statuscode /= 200
        then return $ Fail $ (Error statuscode (Just "") (Just ""))
        else do
            let helper = tryParse json :: (Result CEHelper)
            case helper of
                Fail e -> return $ Fail e
                OK ceh -> return . OK . cehEmbedding $ ceh
