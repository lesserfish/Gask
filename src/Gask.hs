module Gask where

import Gask.API.GenerateContent
import Gask.Types

defaultPart :: String -> Part
defaultPart str = TextPart (Text str)

defaultContent :: String -> Content
defaultContent str = Content [defaultPart str] "user"

defaultRequest :: String -> GenerateContentRequest
defaultRequest str = GenerateContentRequest "AIzaSyABGXkzWRrhN6TAcUS9IXG8HlTbC6XUAoU" "models/gemini-pro" (Just [defaultContent str]) Nothing Nothing
