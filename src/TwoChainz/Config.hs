{-# LANGUAGE OverloadedStrings #-}

module TwoChainz.Config
        ( Config(..)
        , AuthType(..)
        , getConfig
        ) where

import System.Directory (doesFileExist)
import Control.Applicative
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Text  (Text, toLower)
import Data.Yaml

import TwoChainz.Util.Paths (expandUser)

data AuthType = File | Ask -- TODO add more

fromString :: Text -> Maybe AuthType
fromString str = case toLower str of
    "file" -> Just File
    "ask"  -> Just Ask
    _      -> Nothing

instance FromJSON AuthType where
    parseJSON (String v) = return . fromMaybe File $ fromString v
    parseJSON _          = mzero

data Config = Config { keyFile  :: String
                     , authType :: AuthType }

instance FromJSON Config where
    parseJSON (Object v) = Config                          <$>
                           v .:? "keyFile"  .!= "~/.keys"  <*>
                           v .:? "authType" .!= File
    parseJSON _          = mzero

configFile :: FilePath
configFile = "~/.2chainz.yaml"

defaultConfig :: Config
defaultConfig = Config { keyFile  = "~/keys"
                       , authType = File }

decodeIfExists :: FromJSON a => FilePath -> IO (Maybe a)
decodeIfExists filename = do
        exists <- doesFileExist filename
        if exists
            then decodeFile filename
            else return Nothing

getConfig :: IO Config
getConfig = expandUser configFile >>=
            decodeIfExists        >>=
            return . fromMaybe defaultConfig

