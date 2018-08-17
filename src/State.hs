module State where

import Data.IORef

-- |Stores stateful data.
data StateData = StateData
    { stateFilePath :: Maybe FilePath -- ^ The FilePath of the opened/saved file.
    }

-- |Creates a new IORef for StateData.
create :: IO (IORef StateData)
create = newIORef (StateData
    { stateFilePath = Nothing
    })

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
