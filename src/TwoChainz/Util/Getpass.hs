module TwoChainz.Util.Getpass ( getPassword ) where

import System.IO
import Control.Exception

main :: IO ()
main = getPassword >>= putStrLn . ("Entered: " ++)

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
