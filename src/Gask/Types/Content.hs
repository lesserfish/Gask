{-# LANGUAGE OverloadedStrings #-}

module Gask.Types.Content where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)

data Blob = Blob
    { bMimeType :: String
    , bData :: String
    }
    deriving (Show)

instance ToJSON Blob where
    toJSON blob =
        object $
            [ "mimeType" .= (bMimeType blob)
            , "data" .= (bData blob)
            ]

instance FromJSON Blob where
    parseJSON = withObject "Blob" $ \v ->
        Blob
            <$> v .: "mimeType"
            <*> v .: "data"

data Text = Text
    { tData :: String
    }
    deriving (Show)

instance ToJSON Text where
    toJSON text =
        object $
            [ "text" .= (tData text)
            ]

instance FromJSON Text where
    parseJSON = withObject "Text" $ \v ->
        Text
            <$> v .: "text"

data Image = Image
    { iInlineData :: Blob
    }
    deriving (Show)

instance ToJSON Image where
    toJSON image =
        object $
            [ "inlineData" .= (iInlineData image)
            ]

instance FromJSON Image where
    parseJSON = withObject "Image" $ \v ->
        Image
            <$> v .: "inlineData"

data Part = TextPart Text | ImagePart Image deriving (Show)

instance ToJSON Part where
    toJSON (TextPart text) = toJSON text
    toJSON (ImagePart image) = toJSON image

instance FromJSON Part where
    parseJSON value =
        fmap TextPart (parseJSON value :: Parser Text)
            <|> fmap ImagePart (parseJSON value :: Parser Image)

data Content = Content
    { cParts :: [Part]
    , cRole :: String
    }
    deriving (Show)

instance ToJSON Content where
    toJSON content =
        object $
            [ "parts" .= (cParts content)
            , "role" .= (cRole content)
            ]

instance FromJSON Content where
    parseJSON = withObject "Content" $ \v ->
        Content
            <$> v .: "parts"
            <*> v .: "role"

partHasText :: Part -> Bool
partHasText (TextPart _) = True
partHasText _ = False

partText :: Part -> String
partText (ImagePart _) = ""
partText (TextPart (Text txt)) = txt

contentHasText :: Content -> Bool
contentHasText (Content parts _) = (foldl (||) False) . (fmap partHasText) $ parts

contentText :: Content -> String
contentText (Content parts _) = concat . (fmap partText) $ parts

newUserText :: String -> Content
newUserText txt = Content [TextPart (Text txt)] "user"
