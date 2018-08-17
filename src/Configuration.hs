{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Configuration where

import Control.Exception
import Data.Aeson
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics

data ConfigurationData = ConfigurationData
    { configurationLastFile :: Maybe FilePath
    } deriving (Generic, Show)
instance ToJSON ConfigurationData where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ConfigurationData

getLastFile :: ConfigurationData -> Maybe FilePath
getLastFile configuration = configurationLastFile configuration

setLastFile :: ConfigurationData -> Maybe FilePath -> ConfigurationData
setLastFile configuration filePath = configuration { configurationLastFile = filePath }

-- tries to load the configuration and returns the default if it can't
load :: FilePath -> IO (ConfigurationData)
load filePath = do
    let defaultConfig = ConfigurationData { configurationLastFile = Nothing }
    configurationData <- try (TIO.readFile filePath) :: IO (Either SomeException Text)
    pure $ case configurationData of
        Left _ -> defaultConfig
        Right str -> do
            let loadedConfiguration = decode (cs str) :: Maybe ConfigurationData
            case loadedConfiguration of
                Just config -> config
                Nothing     -> defaultConfig

save :: ConfigurationData -> FilePath -> IO (Bool)
save configuration filePath = do
    saved <- try (TIO.writeFile filePath (cs $ encode configuration)) :: IO (Either SomeException ())
    case saved of
        Left _  -> pure False
        Right _ -> pure True
