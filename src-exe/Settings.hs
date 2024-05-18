{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings (Settings (..), loadSettings) where

import Data.Aeson (FromJSON, ToJSON, decode, encode, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as B
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import qualified Gask.API as C
import qualified Gask.API as Gask
import Options.Applicative
import System.Console.ANSI (Color (..))
import System.Directory
import System.FilePath

defaultConfigPath :: String
defaultConfigPath = "~/.config/gask/config.json"

defaultModel :: String
defaultModel = "gemini-pro"

stringToColor :: String -> Color
stringToColor "black" = Black
stringToColor "red" = Red
stringToColor "green" = Green
stringToColor "yellow" = Yellow
stringToColor "blue" = Blue
stringToColor "magenta" = Magenta
stringToColor "cyan" = Cyan
stringToColor _ = White -- Default color is white for unknown strings

expandPath :: FilePath -> IO FilePath
expandPath path =
    case path of
        ('~' : rest) -> do
            homeDir <- getHomeDirectory
            return $ homeDir ++ rest
        _ -> return path

data Configuration = Configuration
    { cKey :: String
    , cModel :: String
    , cSafetySettings :: Maybe [Gask.SafetySetting]
    , cGenerationConfig :: Maybe Gask.GenerationConfig
    }
    deriving (Show, Generic)

instance ToJSON Configuration where
    toJSON c =
        object $
            catMaybes
                [ ("key" .=) <$> (Just . cKey $ c)
                , ("model" .=) <$> (Just . cModel $ c)
                , ("safetySettings" .=) <$> (cSafetySettings $ c)
                , ("generationConfig" .=) <$> (cGenerationConfig $ c)
                ]

instance FromJSON Configuration where
    parseJSON = withObject "Configuration" $ \v ->
        Configuration
            <$> v .: "key"
            <*> v .: "model"
            <*> v .:? "safetySettings"
            <*> v .:? "generationConfig"

data Args = Args
    { aConfigFile :: String
    , aKey :: Maybe String
    , aModel :: Maybe String
    , aInteractive :: Bool
    , aWaitForEOF :: Bool
    , aQuietMode :: Bool
    , aPromptColor :: String
    , aUserColor :: String
    , aModelColor :: String
    , aInput :: [String]
    }
    deriving (Show)

argsParser :: Parser Args
argsParser =
    Args
        <$> strOption
            ( long "config"
                <> short 'c'
                <> metavar "CONFIG_FILE"
                <> value defaultConfigPath
                <> help "Configuration file path"
            )
        <*> optional
            ( strOption
                ( long "key"
                    <> short 'k'
                    <> metavar "KEY"
                    <> help "API key"
                )
            )
        <*> optional
            ( strOption
                ( long "model"
                    <> short 'm'
                    <> metavar "MODEL"
                    <> help "Model"
                )
            )
        <*> switch
            ( long "interactive"
                <> short 'i'
                <> help "Interactive mode"
            )
        <*> switch
            ( long "eofMode"
                <> short 'e'
                <> help "Wait for EOF"
            )
        <*> switch
            ( long "quiet"
                <> short 'q'
                <> help "Don't output prompts or format text"
            )
        <*> strOption
            ( long "prompt-color"
                <> metavar "COLOR"
                <> value "yellow"
                <> help "Color for the prompt"
            )
        <*> strOption
            ( long "user-color"
                <> metavar "COLOR"
                <> value "green"
                <> help "Color for the user text"
            )
        <*> strOption
            ( long "model-color"
                <> metavar "COLOR"
                <> value "blue"
                <> help "Color for the model text"
            )
        <*> many (argument str (metavar "INPUT"))

parseArgs :: IO Args
parseArgs = do
    raw_args <-
        execParser $
            info
                (argsParser <**> helper)
                ( fullDesc
                    <> progDesc "Frontend for Gemini"
                    <> header "Gask"
                )
    config <- (expandPath . aConfigFile $ raw_args) >>= canonicalizePath
    let args = raw_args{aConfigFile = config}
    return args

saveToJsonFile :: (ToJSON a) => FilePath -> a -> IO ()
saveToJsonFile filePath obj = B.writeFile filePath (encode obj)

newConfiguration :: Args -> IO ()
newConfiguration args = do
    let parentDir = takeDirectory . aConfigFile $ args
    createDirectoryIfMissing True parentDir
    putStrLn $ "Creating new configuration file at " ++ (aConfigFile args) ++ "\n\n"
    putStr "API Key: "
    apiKey <- getLine
    let newConfig = Configuration apiKey defaultModel Nothing Nothing
    saveToJsonFile (aConfigFile args) newConfig
    putStrLn $ "\n\nConfiguration file has been created at " ++ (aConfigFile args)
    return ()

loadConfiguration :: Args -> IO (Maybe Configuration)
loadConfiguration args = do
    exists <- doesFileExist . aConfigFile $ args
    if exists
        then do
            fileContent <- B.readFile . aConfigFile $ args
            let maybeConfig = decode fileContent :: Maybe Configuration
            case maybeConfig of
                Nothing -> do
                    putStrLn $ "Failed to parse configuration file."
                    return Nothing
                Just config -> return . Just $ config
        else do
            case (aKey args) of
                Nothing -> do
                    newConfiguration args
                    return Nothing
                Just k -> do
                    let newConfig = Configuration k defaultModel Nothing Nothing
                    return . Just $ newConfig

data Settings = Settings
    { sKey :: String
    , sModel :: String
    , sSafetySettings :: Maybe [C.SafetySetting]
    , sGenerationConfig :: Maybe C.GenerationConfig
    , sInteractive :: Bool
    , sWaitForEOF :: Bool
    , sQuietMode :: Bool
    , sPromptColor :: Color
    , sUserColor :: Color
    , sModelColor :: Color
    , sInput :: String
    }
    deriving (Show)

mergeToSettings :: Args -> Configuration -> Settings
mergeToSettings args config = (Settings key model safety generation interactive eof quietmode cp cu cm input)
  where
    key = case (aKey args) of
        Nothing -> (cKey config)
        Just k -> k
    model = case (aModel args) of
        Nothing -> (cModel config)
        Just m -> m
    safety = cSafetySettings config
    generation = cGenerationConfig config
    interactive = aInteractive args
    eof = aWaitForEOF args
    quietmode = aQuietMode args
    cp = stringToColor . aPromptColor $ args
    cu = stringToColor . aUserColor $ args
    cm = stringToColor . aModelColor $ args
    input = concat . aInput $ args

loadSettings :: IO (Maybe Settings)
loadSettings = do
    args <- parseArgs
    maybeConfig <- loadConfiguration args
    case maybeConfig of
        Nothing -> return Nothing
        Just config -> return . Just $ mergeToSettings args config
