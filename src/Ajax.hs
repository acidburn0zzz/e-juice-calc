{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Ajax where

import qualified Data.Aeson as Aeson
import GHC.Generics
import Network.HTTP.Types
import Network.Wai

-- 
data Result = Result
    { success :: Bool -- if successful
    , message :: Maybe String -- error message
    , content :: Maybe Aeson.Value -- JSON data
    } deriving (Generic, Show)
instance Aeson.ToJSON Result where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance Aeson.FromJSON Result

-- Default headers for Ajax response.
defaultHeaders = [("Content-Type", "application/json")]

make :: Bool -> Maybe String -> Maybe Aeson.Value -> Result
make = Result

makeSuccess :: Maybe Aeson.Value -> Result
makeSuccess = make True Nothing

makeError :: Maybe String -> Result
makeError message = make False message Nothing

toResponseEx :: ResponseHeaders -> Result -> Response
toResponseEx headers result = responseLBS status200 headers (Aeson.encode result)

toResponse :: Result -> Response
toResponse = toResponseEx defaultHeaders
