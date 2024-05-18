{-# LANGUAGE OverloadedStrings #-}

module Gask.API.EmbedContent (EmbedContentRequestInstance (..), EmbedContentRequest (..), embedContent) where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import Data.Maybe (catMaybes)
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

data EmbedContentRequestInstance = EmbedContentRequestInstance
    { ecModelName :: String
    , ecContent :: Content
    , ecTaskType :: Maybe TaskType
    , ecTitle :: Maybe String
    , ecOutputDimensionality :: Maybe Int
    }
    deriving (Show)

data EmbedContentRequest = EmbedContentRequest
    { ecKey :: String
    , ecRequestInstance :: EmbedContentRequestInstance
    }
    deriving (Show)

instance ToJSON EmbedContentRequestInstance where
    toJSON br =
        object $
            catMaybes
                [ ("model" .=) <$> (Just . ecModelName $ br)
                , ("content" .=) <$> (Just . ecContent $ br)
                , ("taskType" .=) <$> (ecTaskType $ br)
                , ("title" .=) <$> (ecTitle $ br)
                , ("outputDimensionality" .=) <$> (ecOutputDimensionality $ br)
                ]

instance FromJSON EmbedContentRequestInstance where
    parseJSON = withObject "EmbedContentRequestInstance" $ \v ->
        EmbedContentRequestInstance
            <$> v .: "model"
            <*> v .: "content"
            <*> v .:? "taskType"
            <*> v .:? "title"
            <*> v .:? "outputDimensionality"

data CEHelper = CEHelper
    { cehEmbedding :: ContentEmbedding
    }
    deriving (Show)

instance ToJSON CEHelper where
    toJSON ceh = object $ ["embedding" .= (cehEmbedding ceh)]

instance FromJSON CEHelper where
    parseJSON = withObject "CEHelper" $ \v -> CEHelper <$> v .: "embedding"

path :: String -> String
path model_name = "/v1/" ++ model_name ++ ":embedContent"

embedContent :: EmbedContentRequest -> IO (Result ContentEmbedding)
embedContent (EmbedContentRequest k br) = do
    let qp = QueryParameters k
    let mn = ecModelName br
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
