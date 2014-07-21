{-# LANGUAGE OverloadedStrings #-}

module TwoChainz.Config
        ( Config(..)
        , AuthType(..)
        , getConfig
        ) where

import Control.Applicative
import Control.Monad
import Data.Char  (toLower)
import Data.Maybe (fromMaybe)
import Data.Yaml

import TwoChainz.Util.Paths (expandUser)

data AuthType = File | Ask -- TODO add more

fromString :: String -> Maybe AuthType
fromString str = case map toLower str of
    "file" -> Just File
    "ask"  -> Just Ask
    _      -> Nothing

data Config = Config { keyFile  :: String
                     , authType :: AuthType }

instance FromJSON Config where
    parseJSON (Object v) = Config                          <$>
                           v .:? "keyFile"  .!= "~/.keys"  <*>
                          (v .:? "authType" >>= return . fromString . fromMaybe "file") .!= File
    parseJSON _          = mzero

configFile :: FilePath
configFile = "~/.2chainz.yaml"

defaultConfig :: Config
defaultConfig = Config { keyFile  = "~/keys"
                       , authType = File }

getConfig :: IO Config
getConfig = expandUser configFile >>= decodeFile >>= return . fromMaybe defaultConfig

