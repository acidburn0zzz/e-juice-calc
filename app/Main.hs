{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Data (Data, Typeable)
import Data.Text (Text)
import Data.String.Conversions
import Data.IORef
import System.Directory
import System.Info
import System.Process
import System.Random
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setOnException)
import Network.HTTP.Types
import Network.Wai

import qualified Configuration as Configuration
import EJuiceCalc
import qualified State as State
import qualified Ajax as Ajax
import qualified JsonRequest as JsonRequest

-- load the inputdata given the configuration
loadInputData :: IORef State.StateData -> Configuration.ConfigurationData -> IO InputData
loadInputData state configuration = do
    let lastFile = Configuration.getLastFile configuration
    case lastFile of
        Just lf -> do
            fileData <- try (readFile lf) :: IO (Either SomeException String)
            case fileData of
                Left _  -> pure $ defaultInputData
                Right fd -> do
                    let decodedInputData = decode (BL8.pack fd) :: Maybe InputData
                    case decodedInputData of
                        Just inputData  -> do
                            State.setFilePath state lf
                            pure $ inputData
                        Nothing -> pure $ defaultInputData
        Nothing -> pure $ defaultInputData

-- |Returns a list of files to search for.
initialDocumentSearch :: String -> [String]
initialDocumentSearch "linux"   = ["/usr/share/e-juice-calc/res/Main.qml", "/usr/local/share/e-juice-calc/res/Main.qml", "res/Main.qml"]
initialDocumentSearch _         = ["res/Main.qml"]

-- |Given a list of file paths, determine which one is usable.
initialDocument :: [String] -> IO (Maybe String)
initialDocument [] = pure Nothing
initialDocument (x:xs) = do
    exists <- doesFileExist x
    case exists of
        True  -> pure $ Just x
        False -> initialDocument xs
        

-- |Main entry point
main :: IO ()
main = do
    -- create the state.
    state <- State.create

    -- create config directory if it doesn't exist.
    configDirectory <- getXdgDirectory XdgConfig "e-juice-calc"
    createDirectoryIfMissing True configDirectory

    let configFilePath = configDirectory ++ "/config.json"

    -- try to load the configuration.
    configuration <- Configuration.load configFilePath

    -- load the inputdata given the configuration.
    inputData <- loadInputData state configuration

    -- grab a random value.
    randomValue <- randomIO :: IO Int
    let port = (randomValue `mod` 50000) + 10000 -- range: 10000-60000

    -- our main qml file to load.
    fp <- initialDocument $ initialDocumentSearch os
    case fp of
        Nothing       -> pure ()
        Just filePath -> do
            -- launch process with appropriate arguments.
            bracket
                -- create our process.
                (spawnProcess "qmllb" [show port, filePath])
                -- kill the process.
                (\pid -> (try $ terminateProcess pid :: IO (Either SomeException ())) >> pure ())
                -- run our webserver and wait for shutdown signal.
                (\_ -> do
                    shutdownSignal <- State.getShutdown state
                    race_ (takeMVar shutdownSignal) (runSettings warpSettings (app inputData state)))
            -- save the configuration.
            lastFilePath <- State.getFilePath state
            fileSaved <- Configuration.save (Configuration.setLastFile configuration lastFilePath) configFilePath
            -- TODO: check if not saved and handle error
            pure ()
            where
                exceptionHandler :: Maybe Request -> SomeException -> IO ()
                exceptionHandler _ _ = pure ()
                warpSettings = setOnException exceptionHandler $ setPort port defaultSettings

app :: InputData -> IORef State.StateData -> Application
app initInputData state request respond = do
    reqBody <- strictRequestBody request
    response <- case rawPathInfo request of
        "/init"      -> haskellInit initInputData
        "/open"      -> haskellOpen state reqBody
        "/save"      -> haskellSave state reqBody
        "/filepath"  -> haskellGetFilePath state
        "/calculate" -> haskellCalculate reqBody
        "/version"   -> haskellVersion
        "/exit"      -> haskellExit state
        _            -> pure $ Ajax.toResponse $ Ajax.makeError (Just "Not Found.")
    respond response

-- the default headers
defaultHeaders = [("Content-Type", "application/json")]

-- helper function
tryDecode :: FromJSON a => String -> ExceptT String IO a
tryDecode str = do
    let result = decode (BL8.pack str)
    case result of
        Nothing -> throwE "Failed to decode the request body."
        Just x  -> ExceptT (pure (Right x))


haskellInit :: InputData -> IO Response
haskellInit inputData = pure $ Ajax.toResponse $ Ajax.makeSuccess (decode $ encode inputData) -- TODO: FIXME: encode...decode

-- read file and validate
haskellOpen :: IORef State.StateData -> BL.ByteString -> IO Response
haskellOpen state requestBody = do
    result <- runExceptT $ do 
        reqObj <- tryDecode (cs requestBody) :: ExceptT String IO JsonRequest.Open
        let filePath = JsonRequest.getOpenFilePath reqObj
        fileData <- tryReadFile filePath
        inputData <- tryDecode fileData :: ExceptT String IO InputData
        ExceptT (pure $ Right (filePath, inputData))
    case result of
        Left message -> pure $ Ajax.toResponse $ Ajax.makeError (Just message)
        Right (filePath, inputData)      -> do
            State.setFilePath state (cs filePath)
            pure $ Ajax.toResponse $ Ajax.makeSuccess (decode $ encode inputData) -- TODO: FIXME: encode...decode
    where
        parseRequest :: BL.ByteString -> ExceptT String IO String
        parseRequest requestBody = do
            let filePath = decode requestBody :: Maybe String
            case filePath of
                Just x -> pure x
                _      -> throwE "Failed to parse request body." 
        tryReadFile :: String -> ExceptT String IO String
        tryReadFile filePath = join $ liftIO $ try (readFile (cs filePath)) >>= convReadException
            where
                convReadException :: Either SomeException String -> IO (ExceptT String IO String)
                convReadException x = pure $ case x of
                    Left _ -> throwE "Failed to read file."
                    Right xx -> ExceptT (pure (Right xx))

-- validate and save to file
-- state, filepath, inputstr
haskellSave :: IORef State.StateData -> BL.ByteString -> IO Response
haskellSave state requestBody = do
    result <- runExceptT $ do
        reqObj <- tryDecode (cs requestBody) :: ExceptT String IO JsonRequest.Save
        trySaveFile (JsonRequest.getSaveFilePath reqObj) (cs $ encode $ JsonRequest.getSaveInputData reqObj)
    pure $ case result of
        Left  msg -> Ajax.toResponse $ Ajax.makeError (Just msg)
        Right _   -> Ajax.toResponse $ Ajax.makeSuccess Nothing
    where
        trySaveFile :: String -> String -> ExceptT String IO ()
        trySaveFile filePath str = join $ liftIO $ try (writeFile filePath str) >>= convWriteException
            where
                convWriteException :: Either SomeException () -> IO (ExceptT String IO ())
                convWriteException x = pure $ case x of
                    Left _ -> throwE "Failed to write to file."
                    Right xx -> ExceptT (pure (Right xx))

-- get the filepath of the last opened/saved file
haskellGetFilePath :: IORef State.StateData -> IO Response
haskellGetFilePath state = do
    filePath <- State.getFilePath state
    pure $ Ajax.toResponse $ Ajax.makeSuccess (decode $ encode filePath) -- TODO: FIXME: encode...decode

-- calculate the recipe
haskellCalculate :: BL.ByteString -> IO Response
haskellCalculate requestBody = do
    result <- runExceptT $ (tryDecode $ cs requestBody :: ExceptT String IO InputData)
    pure $ case result of
        Left  msg       -> Ajax.toResponse $ Ajax.makeError (Just msg)
        Right inputData -> Ajax.toResponse $ Ajax.makeSuccess (decode $ (encode $ calc inputData)) -- TODO: FIXME: encode...decode

-- get the version of the program
haskellVersion :: IO Response
haskellVersion = pure $ Ajax.toResponse $ Ajax.makeSuccess (decode $ encode version) -- TODO: FIXME: encode...decode

data Shutdown = Shutdown deriving (Data, Typeable, Show)
instance Exception Shutdown

-- exit by setting the shutdown signal
haskellExit :: IORef State.StateData -> IO Response
haskellExit state = do
    State.setShutdown state
    pure $ Ajax.toResponse $ Ajax.makeSuccess Nothing
