module Main where

import System.Environment
import Data.List

main :: IO ()
main = getArgs >>= putStrLn . intercalate ", "
