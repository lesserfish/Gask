# Documentation

I will describe minimally how to use the Gask library to perform Gemini API calls.

Gask uses Gemini's Rest API. If you read the [official documentation](https://ai.google.dev/api/rest), you will find there is not a lot going on in this library. It is pretty much an almost replica of the official REST API and nothing else.

The following API calls can be requested:

 - embedContent: Generates an embedding from the model given an input.
 - batchEmbedContent: Generates multiple embeddings from the model given input text in a synchronous call.
 - get: Gets information about a specific Model.
 - list: Lists models available through the API.
 - generateContent: Generates a response from the model given an input.
 - streamGenerateContent: Generates a streamed response from the model given an input.


Each API method has its own .hs file located in Gask/API. Each API method has its own respective call. These include:

    embedContent :: EmbedContentRequest -> IO (Result ContentEmbedding)
    batchEmbedContents :: BatchEmbedContentsRequest -> IO (Result [ContentEmbedding])
    getModelInfo :: GetRequest -> IO (Result ModelInfo)
    getModelList :: ListRequest -> IO (Result ModelList)
    generateContent :: GenerateContentRequest -> IO (Result GenerateContentResponse
    streamGenerateContent :: StreamGenerateContentRequest -> ((Result GenerateContentResponse) -> IO ()) -> IO ()
    streamGenerateContentMC :: (Monoid a) => StreamGenerateContentRequest -> ((Result GenerateContentResponse) -> IO a) -> IO a
    streamGenerateContentC :: StreamGenerateContentRequest -> (ConduitT (Result GenerateContentResponse) Void IO a) -> IO a


   Honestly, a quick glance on the definition of each Request and Response should be enough to understand how to use this.

   Therefore, I will only cover the most interesting API points: generateContent and streamGenerateContent.

   ## generateContent

   generateContent is the API call that will get a response from Gemini Model. To call this method, you will need the following type as input:

    data GenerateContentRequest = GenerateContentRequest
    { gcKey :: String
    , gcModel :: String
    , gcContents :: Maybe [Content]
    , gcSafetySettings :: Maybe [SafetySetting]
    , gcGenerationConfig :: Maybe GenerationConfig
    }
    deriving (Show)

You will need to specify the API key, the model to be used, the content of the message. You may also include [safety settings](https://ai.google.dev/api/rest/v1/SafetySetting) and [generation settings](https://ai.google.dev/api/rest/v1/GenerationConfig). To understand these last two, please refer to the official documentation.

The definition of Content is as follows:

    
    data Content = Content
        { cParts :: [Part]
        , cRole :: String
        }
        deriving (Show)
    
    
    data Part = TextPart Text | ImagePart Image deriving (Show)
    
    data Text = Text
        { tData :: String
        }
        deriving (Show)
    

So far, only text requests are supported.

cParts should contain an array of messages. 

cRole should contain a string establishing the source of the message: Either 'user' or 'model'.

You can easily create an user text message by running

    newUserText :: String -> Content

The output of generateContent is a Result GenerateContentResponse. A Result is defined as

    data Result a = OK a | Fail Error

It should be pretty self-explanatory.

GenerateContentResponse is defined as 

    data GenerateContentResponse = GenerateContentResponse
        { gcrCandidates :: [Candidate]
        , gcrPromptFeedback :: Maybe PromptFeedback
        , gcrUsageMetadata :: Maybe UsageMetadata
        }
        deriving (Show)

The list of candidates will include the text response (when it exists). Candidate is defined as


    data Candidate = Candidate
        { cContent :: Maybe Content
        , cFinishReason :: Maybe FinishReason
        , cSafetyRatings :: Maybe [SafetyRating]
        , cCitationMetadata :: Maybe CitationMetadata
        , cTokenCount :: Maybe Int
        , cIndex :: Maybe Int
        }
        deriving (Show)

The text content is included in cContent, and can be obtained with the auxiliary functions:

    candidateHasText :: Candidate -> Bool
    
    candidateText :: Candidate -> String

Other information about the response can be seen in the other attributes. Read the official documentation and the .hs files to understand how to work with them.

All in all, there should be no surprises in this haskell code. It is the most barebones implementation you could imagine.


## streamGenerateResponse

streamGenerateResponse allows you to generate response, with the caviat that the data comes in a list of shorter responses, allowing you to stream its content continuously to output.

There are three methods that allow you to stream responses from Gemini:

    streamGenerateContent :: StreamGenerateContentRequest -> ((Result GenerateContentResponse) -> IO ()) -> IO ()
    
    streamGenerateContentMC :: (Monoid a) => StreamGenerateContentRequest -> ((Result GenerateContentResponse) -> IO a) -> IO a
    
    streamGenerateContentC :: StreamGenerateContentRequest -> (ConduitT (Result GenerateContentResponse) Void IO a) -> IO a


streamGenerateContent is the simplest. It requires a streamGenerateContentRequest (an lias of generateContentRequest), and a function:

    render :: ((Result GenerateContentResponse) -> IO ())

This function should render the responses from Gemini. You should use this method when all you want to do is display the results, and are not interested in anything else.

Alternatively, you could use streamGenerateContentMC. The following is a small render function that uses this type signature:

    defaultRender :: Settings -> (G.Result G.GenerateContentResponse) -> IO [G.GenerateContentResponse]
    defaultRender settings (G.Fail e) = do
        let quietMode = sQuietMode settings
        if quietMode
            then return []
            else do
                renderError e
                return []
    defaultRender settings (G.OK response) = do
        let quietMode = sQuietMode settings
        if G.responseHasText response
            then do
                if quietMode
                    then do
                        putStr $ G.responseText response
                        return [response]
                    else do
                        setColor . sModelColor $ settings
                        putStr $ G.responseText response
                        resetColor
                        return [response]
            else do
                if quietMode
                    then return []
                    else do
                        putStr $ "[No response]"
                        return []


If you were to run

    outputs <- streamGenerateContentMC request (defaultRender settings)

outputs would be filled with all of the valid responses from Gemini. You could then use this information to establish whether or not to continue your main loop or not.

I will leave the usage of streamGenerateContentC to those who are familiar with conduits.

## More

Nothing here is magic. I'm not good at Haskell. Read the official documentation if you are uncertain about something. 

