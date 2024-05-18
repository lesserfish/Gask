{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Configuration where

import Data.Aeson (FromJSON, ToJSON, decode, encode, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as B
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import qualified Gask.API as Gask
import Options.Applicative
import System.Directory
import System.FilePath

defaultConfigPath :: String
defaultConfigPath = "~/.config/gask/config.json"

defaultModel :: String
defaultModel = "gemini-pro"

expandPath :: FilePath -> IO FilePath
expandPath path =
    case path of
        ('~' : rest) -> do
            homeDir <- getHomeDirectory
            return $ homeDir ++ rest
        _ -> return path

data Configuration = Configuration
    { key :: String
    , model :: String
    , safetySettings :: Maybe [Gask.SafetySetting]
    , generationConfig :: Maybe Gask.GenerationConfig
    }
    deriving (Show, Generic)

instance ToJSON Configuration where
    toJSON c =
        object $
            catMaybes
                [ ("key" .=) <$> (Just . key $ c)
                , ("model" .=) <$> (Just . model $ c)
                , ("safetySettings" .=) <$> (safetySettings $ c)
                , ("generationConfig" .=) <$> (generationConfig $ c)
                ]

instance FromJSON Configuration where
    parseJSON = withObject "Configuration" $ \v ->
        Configuration
            <$> v .: "key"
            <*> v .: "model"
            <*> v .:? "safetySettings"
            <*> v .:? "generationConfig"

data Args = Args
    { configFile :: String
    , aKey :: Maybe String
    , aModel :: Maybe String
    , interactive :: Bool
    , waitForEOF :: Bool
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
    config <- (expandPath . configFile $ raw_args) >>= canonicalizePath
    let args = raw_args{configFile = config}
    return args

saveToJsonFile :: (ToJSON a) => FilePath -> a -> IO ()
saveToJsonFile filePath obj = B.writeFile filePath (encode obj)

newConfiguration :: Args -> IO ()
newConfiguration args = do
    let parentDir = takeDirectory . configFile $ args
    createDirectoryIfMissing True parentDir
    putStrLn $ "Creating new configuration file at " ++ (configFile args) ++ "\n\n"
    putStr "API Key: "
    apiKey <- getLine
    let newConfig = Configuration apiKey defaultModel Nothing Nothing
    saveToJsonFile (configFile args) newConfig
    putStrLn $ "\n\nConfiguration file has been created at " ++ (configFile args)
    return ()

overloadConfiguration :: Args -> Configuration -> Configuration
overloadConfiguration args config = config{key = okey, model = omodel}
  where
    okey = case (aKey args) of
        Nothing -> (key config)
        Just k -> k
    omodel = case (aModel args) of
        Nothing -> (model config)
        Just m -> m

loadConfiguration :: Args -> IO (Maybe Configuration)
loadConfiguration args = do
    exists <- doesFileExist . configFile $ args
    if exists
        then do
            fileContent <- B.readFile . configFile $ args
            let maybeConfig = decode fileContent :: Maybe Configuration
            case maybeConfig of
                Nothing -> do
                    putStrLn $ "Failed to parse configuration file."
                    return Nothing
                Just config -> return . Just . (overloadConfiguration args) $ config
        else do
            case (aKey args) of
                Nothing -> do
                    newConfiguration args
                    return Nothing
                Just k -> do
                    let newConfig = Configuration k defaultModel Nothing Nothing
                    return . Just . (overloadConfiguration args) $ newConfig
