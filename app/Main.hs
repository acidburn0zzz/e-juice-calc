{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String.Conversions
import Data.IORef
import Graphics.QML
import System.Directory

import qualified Configuration as Configuration
import qualified State as State
import EJuiceCalc

-- load the inputdata given the configuration
loadInputData :: IORef State.StateData -> Configuration.ConfigurationData -> IO (InputData)
loadInputData state configuration = do
    let lastFile = Configuration.getLastFile configuration
    case lastFile of
        Just lf -> do
            fileData <- try (readFile lf) :: IO (Either SomeException String)
            case fileData of
                Left _  -> pure $ defaultInputData
                Right fd -> do
                    let decodedInputData = decode (cs fd) :: Maybe InputData
                    case decodedInputData of
                        Just inputData  -> do
                            State.setFilePath state lf
                            pure $ inputData
                        Nothing -> pure $ defaultInputData
        Nothing -> pure $ defaultInputData

-- main entry point
main :: IO ()
main = do
    -- create the state
    state <- State.create

    -- create config directory if it doesn't exist
    configDirectory <- getXdgDirectory XdgConfig "e-juice-calc"
    createDirectoryIfMissing True configDirectory

    let configFilePath = configDirectory ++ "/config.json"

    -- try to load the configuration
    configuration <- Configuration.load configFilePath

    -- load the inputdata given the configuration
    inputData <- loadInputData state configuration

    -- create our class with the haskell functions
    mainClass <- newClass [
        defMethod' "haskellInit" (\_ -> pure $ cs $ encode inputData :: IO Text),
        defMethod' "haskellOpen" (\_ filePath -> haskellOpen state filePath),
        defMethod' "haskellSave" (\_ filePath inputStr -> haskellSave state filePath inputStr),
        defMethod' "haskellGetFilePath" (\_ -> haskellGetFilePath state),
        defMethod' "haskellCalc" (\_ input -> haskellCalc input),
        defMethod' "haskellGetVersion" (\_ -> haskellGetVersion)]
    
    -- create a new instance of our class
    ctx <- newObject mainClass ()
    
    -- run the GUI
    runEngineLoop defaultEngineConfig {
          initialDocument = fileDocument "res/Main.qml"
        , contextObject = Just $ anyObjRef ctx
    }

    -- save the configuration
    lastFilePath <- State.getFilePath state
    fileSaved <- Configuration.save (Configuration.setLastFile configuration lastFilePath) configFilePath
    -- TODO: check if not saved and handle error
    
    pure ()

-- read file and validate
haskellOpen :: IORef State.StateData -> Text -> IO (Maybe Text)
haskellOpen state filePath = do
    -- the file should exists because it has either
    -- been loaded from the configuration or the user selected it in a dialog
    result <- try (readFile (cs filePath)) :: IO (Either SomeException String)
    let inputStr = case result of
                   Left _    -> encode (Nothing :: Maybe Text)
                   Right str -> cs str
    -- validate json and update state
    let inputData = decode (cs inputStr) :: Maybe InputData
    case inputData of
        Just x  -> do
            State.setFilePath state (cs filePath)
            pure $ Just $ cs inputStr
        Nothing -> pure Nothing

-- validate and save to file
haskellSave :: IORef State.StateData -> Text -> Text -> IO Bool
haskellSave state filePath inputStr = do
    -- validate json
    let inputData = decode (cs $ inputStr) :: Maybe InputData
    case inputData of
        Just x -> do
            -- valid - try to write the file
            result <- try (writeFile (cs filePath) (cs inputStr)) :: IO (Either SomeException ())
            case result of
                Left _ -> pure False
                Right _ -> do
                    State.setFilePath state (cs filePath)
                    pure True
        Nothing -> pure False

-- get the filepath of the last opened/saved file
haskellGetFilePath :: IORef State.StateData -> IO (Maybe Text)
haskellGetFilePath state = do
    filePath <- State.getFilePath state
    pure $ case filePath of
        Just fp -> Just $ cs $ encode fp
        Nothing -> Nothing

-- calculate the recipe
haskellCalc :: Text -> IO Text
haskellCalc input = do
    let inputData = decode (cs input) :: Maybe InputData
    pure $ case inputData of
        Just x  -> cs $ encode (calc x)
        Nothing -> cs "null"

-- get the version of the program
haskellGetVersion :: IO Text
haskellGetVersion = pure $ cs version
