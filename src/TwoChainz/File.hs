{-# LANGUAGE OverloadedStrings #-}

module TwoChainz.File
        ( encryptToFile
        , decryptFile
        , Key
        ) where

import Control.Applicative
import TwoChainz.Types (Key)
import Crypto.Random.DRBG
import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

blockSize :: Int
blockSize = 16

pad :: B.ByteString -> B.ByteString
pad str =
    let padding = case B.length str `mod` blockSize of
            0 -> 0
            x -> blockSize - x
    in str `B.append` BC.replicate padding '\0'

unpad :: B.ByteString -> B.ByteString
unpad ""  = ""
unpad str = case BC.last str of
    '\0' -> unpad $ BC.init str
    _    -> str

encryptToFile :: Key -> FilePath -> B.ByteString -> IO ()
encryptToFile key path plaintext = do
    Right (iv, _) <- genBytes 128 <$> (newGenIO :: IO HashDRBG)

    let ciphertext = B.append iv $
            AES.encryptCBC (AES.initAES key) iv . pad $ plaintext
    B.writeFile path ciphertext

decryptFile :: Key -> FilePath -> IO B.ByteString
decryptFile key path = do
    (iv, plaintext) <- B.splitAt 128 <$> B.readFile path
    let context = AES.initAES key
    return $ unpad . AES.decryptCBC context iv . pad $ plaintext


-- vim:ts=4 sts=4 sw=4:

