module TwoChainz.File
        ( encryptFile
        , decryptFile
        )
    where

import System.IO
import Data.ByteString
import qualified Crypto.Cypher.AES.Haskell as AES

encryptFile :: FilePath -> ByteString
encryptFile = undefined

