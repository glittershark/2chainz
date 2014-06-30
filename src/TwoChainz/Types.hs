module TwoChainz.Types
    ( Key
    , Password
    , Account
    , Pair
    ) where

import qualified Data.ByteString as B

type Key = B.ByteString
type Password = B.ByteString
type Account = B.ByteString
type Pair = (Password, Account)

