module Gask where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Gask.API as G
import Settings

type History = [G.Content]

data Gask = Gask
    { gSettings :: Settings
    , gHistory :: History
    }
    deriving (Show)

clearHistory :: (Monad m) => StateT Gask m ()
clearHistory = modify (\gask -> gask{gHistory = []})

chatT :: Bool -> (Gask -> IO String) -> ((G.Result G.GenerateContentResponse) -> IO [G.GenerateContentResponse]) -> StateT Gask IO ()
chatT stopOnEmpty fetch render = do
    gask <- get
    message <- lift . fetch $ gask
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

chat :: Bool -> Settings -> (Gask -> IO String) -> ((G.Result G.GenerateContentResponse) -> IO [G.GenerateContentResponse]) -> IO Gask
chat stopOnEmpty settings fetch render = do
    let emptyGask = Gask settings []
    (_, gask) <- runStateT (chatT stopOnEmpty fetch render) emptyGask
    return gask

chatForever :: Settings -> (Gask -> IO String) -> ((G.Result G.GenerateContentResponse) -> IO [G.GenerateContentResponse]) -> IO Gask
chatForever = chat True
