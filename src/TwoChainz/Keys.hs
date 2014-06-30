{-# LANGUAGE OverloadedStrings #-}
module TwoChainz.Keys ( getKey )
    where

import TwoChainz.Types (Key)

getKey :: IO Key
getKey = return "testasdfadsfasdf"

