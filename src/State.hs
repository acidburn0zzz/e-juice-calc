module State where

import Control.Concurrent.MVar
import Data.IORef

-- |Stores stateful data.
data StateData = StateData
    { stateShutdown :: MVar Bool
    , stateFilePath :: Maybe FilePath -- ^ The FilePath of the opened/saved file.
    }

-- |Creates a new IORef for StateData.
create :: IO (IORef StateData)
create = do
    shutdown <- newEmptyMVar
    newIORef (StateData
        { stateShutdown = shutdown
        , stateFilePath = Nothing
        })

setShutdown :: IORef StateData -> IO ()
setShutdown state = do
    s <- readIORef state
    putMVar (stateShutdown s) True

getShutdown :: IORef StateData -> IO (MVar Bool)
getShutdown state = do
    s <- readIORef state
    pure $ stateShutdown s 
    
-- |Sets the FilePath in the State.
setFilePath :: IORef StateData -> FilePath -> IO ()
setFilePath state filePath = do
    s <- readIORef state
    writeIORef state $ s { stateFilePath = Just filePath }

-- |Gets the FilePath from the state.
getFilePath :: IORef StateData -> IO (Maybe FilePath)
getFilePath state = do 
    s <- readIORef state
    pure $ stateFilePath s
