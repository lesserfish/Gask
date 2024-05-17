module Gask where

import Gask.Types

defaultPart :: Part
defaultPart = TextPart (Text "Hello! Can you tell me a poem?")

defaultContent :: Content
defaultContent = Content [defaultPart] "user"
