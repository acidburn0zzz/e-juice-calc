module State where

import Data.IORef

data StateData = StateData
    -- the filepath of the opened/saved file
    { stateFilePath :: Maybe FilePath
    }

create :: IO (IORef StateData)
create = newIORef (StateData
    { stateFilePath = Nothing
    })

setFilePath :: IORef StateData -> FilePath -> IO ()
setFilePath state filePath = do
    s <- readIORef state
    writeIORef state $ s { stateFilePath = Just filePath }

getFilePath :: IORef StateData -> IO (Maybe FilePath)
getFilePath state = do 
    s <- readIORef state
    pure $ stateFilePath s
