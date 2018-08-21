{-# LANGUAGE DeriveGeneric #-}

module Configuration where

import Control.Exception
import Data.Aeson
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

data ConfigurationData = ConfigurationData
    { lastFile :: Maybe FilePath
    } deriving (Generic, Show)
instance ToJSON ConfigurationData where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ConfigurationData

-- |Gets the last file which has been used.
getLastFile :: ConfigurationData -> Maybe FilePath
getLastFile = lastFile

-- |Sets the last file which has been used.
setLastFile :: ConfigurationData -> Maybe FilePath -> ConfigurationData
setLastFile configuration filePath = configuration { lastFile = filePath }

-- |Loads a configuration and returns the default if unsuccessful.
load :: FilePath -> IO ConfigurationData
load filePath = do
    let defaultConfig = ConfigurationData { lastFile = Nothing }
    configurationData <- try (readFile filePath) :: IO (Either SomeException String)
    pure $ case configurationData of
        Left _ -> defaultConfig
        Right str -> do
            let loadedConfiguration = decode (cs str) :: Maybe ConfigurationData
            case loadedConfiguration of
                Just config -> config
                Nothing     -> defaultConfig

-- |Saves a configuration given a FilePath.
save :: ConfigurationData -> FilePath -> IO Bool
save configuration filePath = do
    saved <- try (writeFile filePath (cs $ encode configuration)) :: IO (Either SomeException ())
    case saved of
        Left _  -> pure False
        Right _ -> pure True
