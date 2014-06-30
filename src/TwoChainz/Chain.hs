module TwoChainz.Chain
    ( writePassword
    , retrievePassword
    ) where

import Control.Applicative
import System.Directory
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import TwoChainz.Types (Password, Account, Pair)
import TwoChainz.File (encryptToFile, decryptFile)
import TwoChainz.Keys (getKey)

type Chain = A.Object

addPassword :: Chain -> Pair -> Chain
addPassword chain (account, pass) = M.insert account' pass' chain
    where account' = T.pack . B.unpack $ account
          pass' = A.String . T.pack . B.unpack $ pass

getPassword :: Chain -> Account -> Maybe Password
getPassword chain account = B.pack . T.unpack . extractString <$> M.lookup account' chain
    where account' = T.pack . B.unpack $ account
          -- TODO handle non-String values with `Nothing`
          extractString val = let A.String txt = val in txt

-- Monadic password IO

writePassword :: FilePath -> Pair -> IO ()
writePassword path pair = do
    key    <- getKey
    exists <- doesFileExist path
    chain  <- if exists then do
                source <- decryptFile key path
                return $ case A.decode . BL.fromStrict $ source of
                    Just chain -> chain
                    Nothing    -> M.empty
              else return M.empty

    encryptToFile key path $ BL.toStrict . A.encode $ addPassword chain pair

retrievePassword :: FilePath -> Account -> IO (Maybe Password)
retrievePassword path account = do
    key <- getKey
    source <- decryptFile key path
    return $ do -- Maybe monad!
        chain <- A.decode . BL.fromStrict $ source
        getPassword chain account


