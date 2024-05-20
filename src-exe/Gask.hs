module Gask (
    Gask (..),
    History,
    clearHistory,
    chatT,
    chat,
    chatForever,
    chatFromStringT,
    chatFromString,
    resumeChat,
    resumeChatFromString,
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Gask.API as G
import Settings

type Response = G.GenerateContentResponse
type History = [G.Content]

type Fetch = Gask -> IO (Maybe String)
type Render = (G.Result Response) -> IO [Response]

data Gask = Gask
    { gSettings :: Settings
    , gHistory :: History
    }
    deriving (Show)

clearHistory :: (Monad m) => StateT Gask m ()
clearHistory = modify (\gask -> gask{gHistory = []})

chatT :: Bool -> Fetch -> Render -> StateT Gask IO ()
chatT stopOnEmpty fetch render = do
    gask <- get
    maybemessage <- lift . fetch $ gask
    case maybemessage of
        Nothing -> return ()
        Just message -> do
            let userContent = G.newUserText message
            let fullContent = (gHistory gask) ++ [userContent]
            let request =
                    G.GenerateContentRequest
                        (sKey . gSettings $ gask)
                        (sModel . gSettings $ gask)
                        (Just fullContent)
                        (sSafetySettings . gSettings $ gask)
                        (sGenerationConfig . gSettings $ gask)

            responses <- lift $ G.streamGenerateContentMC request render
            let responseCount = length responses
            let modelContent = concat . (fmap G.responseContents) $ responses
            put gask{gHistory = fullContent ++ modelContent}

            if stopOnEmpty && responseCount == 0
                then return ()
                else chatT stopOnEmpty fetch render

chat :: Bool -> Settings -> Fetch -> Render -> IO Gask
chat stopOnEmpty settings fetch render = do
    let emptyGask = Gask settings []
    (_, gask) <- runStateT (chatT stopOnEmpty fetch render) emptyGask
    return gask

chatForever :: Settings -> Fetch -> Render -> IO Gask
chatForever = chat True

resumeChat :: History -> Bool -> Settings -> Fetch -> Render -> IO Gask
resumeChat history stopOnEmpty settings fetch render = do
    let previousGask = Gask settings history
    (_, gask) <- runStateT (chatT stopOnEmpty fetch render) previousGask
    return gask

chatFromStringT :: String -> Bool -> Fetch -> Render -> StateT Gask IO ()
chatFromStringT message stopOnEmpty fetch render = do
    gask <- get
    let userContent = G.newUserText message
    let fullContent = (gHistory gask) ++ [userContent]
    let request =
            G.GenerateContentRequest
                (sKey . gSettings $ gask)
                (sModel . gSettings $ gask)
                (Just fullContent)
                (sSafetySettings . gSettings $ gask)
                (sGenerationConfig . gSettings $ gask)

    responses <- lift $ G.streamGenerateContentMC request render
    let responseCount = length responses
    let modelContent = concat . (fmap G.responseContents) $ responses
    put gask{gHistory = fullContent ++ modelContent}

    if stopOnEmpty && responseCount == 0
        then return ()
        else chatT stopOnEmpty fetch render

chatFromString :: String -> Bool -> Settings -> Fetch -> Render -> IO Gask
chatFromString message stopOnEmpty settings fetch render = do
    let previousGask = Gask settings []
    (_, gask) <- runStateT (chatFromStringT message stopOnEmpty fetch render) previousGask
    return gask

resumeChatFromString :: History -> String -> Bool -> Settings -> Fetch -> Render -> IO Gask
resumeChatFromString history message stopOnEmpty settings fetch render = do
    let previousGask = Gask settings history
    (_, gask) <- runStateT (chatFromStringT message stopOnEmpty fetch render) previousGask
    return gask
