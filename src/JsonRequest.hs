{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module JsonRequest where

import GHC.Generics
import qualified Data.Aeson as Aeson

import EJuiceCalc

data Open = Open
    { filePath :: String
    } deriving (Generic, Show)
instance Aeson.ToJSON Open where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance Aeson.FromJSON Open

getOpenFilePath :: Open -> String
getOpenFilePath = filePath

data Save = Save
    { filePath :: String
    , inputData :: InputData
    } deriving (Generic, Show)
instance Aeson.ToJSON Save where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance Aeson.FromJSON Save

getSaveFilePath :: Save -> String
getSaveFilePath = filePath

getSaveInputData :: Save -> InputData
getSaveInputData = inputData
