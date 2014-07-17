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

data AuthType = File | Ask -- TODO add more

defaultType :: AuthType
defaultType = File

fromString :: String -> Maybe AuthType
fromString str = case map toLower str of
    "file" -> Just File
    "ask"  -> Just Ask
    _      -> Nothing

data Config = Config { keyFile  :: String
                     , authType :: AuthType
                     }

instance FromJSON Config where
    parseJSON (Object v) = Config                          <$>
                           v .:? "keyFile"  .!= "~/.keys"  <*>
                           (v .:? "authType" >>= return . fromString . fromMaybe "file") .!= File
    parseJSON _          = mzero


