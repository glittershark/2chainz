{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (stderr, hPutStrLn)
import Control.Monad
import Control.Applicative
import Data.Bifunctor
import System.Environment
import Safe
import qualified Data.ByteString.Char8 as BC

import TwoChainz.Chain
import TwoChainz.Types
import TwoChainz.Util.Paths   (expandUser)
import TwoChainz.Util.Getpass (getPassword)

getChainFile :: IO FilePath
getChainFile = expandUser "~/.keychain"

invalidArgs :: IO ()
invalidArgs = putStrLn "Invalid number of arguments"


get :: [String] -> IO ()
get args = if length args /= 1 then invalidArgs else
    let (account:[]) = args in do
        chainFile <- getChainFile
        result  <- retrievePassword chainFile . BC.pack $ account
        case result of
            Just password -> putStrLn $ BC.unpack password
            Nothing       -> hPutStrLn stderr "Not found"


parseSetArgs :: [String] -> IO (Account, Password)
parseSetArgs args = join bimap BC.pack <$> case args of
    (account:[])          -> (,) account <$> getPassword
    (account:password:[]) -> return (account, password)
    _                     -> undefined

set :: [String] -> IO ()
set args = if length args > 2 then invalidArgs else do
    pair <- parseSetArgs args
    chainFile <- getChainFile
    writePassword chainFile pair

    let account = BC.unpack $ fst pair
    putStrLn $ "Wrote password for account \"" ++ account ++ "\""

main :: IO ()
main = do
    args <- getArgs
    case headMay args of
        Just "get"      -> get $ tail args
        Just "set"      -> set $ tail args
        Just "shoutout" -> putStrLn "What up, wrist!"
        Just command    -> putStrLn $ "Unrecognized command: " ++ command
        Nothing         -> invalidArgs

