{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (stderr, hPutStrLn)
import Control.Monad
import Data.Bifunctor
import System.Environment
import Safe
import qualified Data.ByteString.Char8 as BC

import TwoChainz.Chain

keyFile :: String
keyFile = "/home/smith/.keys"

invalidArgs :: IO ()
invalidArgs = putStrLn "Invalid number of arguments"


get :: [String] -> IO ()
get args = if length args /= 1 then invalidArgs else
    let (account:[]) = args in do
        result <- retrievePassword keyFile . BC.pack $ account
        case result of
            Just password -> putStrLn $ BC.unpack password
            Nothing       -> hPutStrLn stderr "Not found"

set :: [String] -> IO ()
set args = if length args /= 2 then invalidArgs else
    let (account:password:[]) = args in do
        writePassword keyFile $ join bimap BC.pack $ (account, password)
        putStrLn $ "Wrote password for account \"" ++ account ++ "\""

main :: IO ()
main = do
    args <- getArgs
    case headMay args of
        Just "get"   -> get $ tail args
        Just "set"   -> set $ tail args
        Just command -> putStrLn $ "Unrecognized command: " ++ command
        Nothing      -> invalidArgs

