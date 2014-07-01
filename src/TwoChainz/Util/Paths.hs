module TwoChainz.Util.Paths
    ( expandUser
    ) where

import System.FilePath.Posix
import System.Directory

expandUser :: FilePath -> IO FilePath
expandUser "~"         = getHomeDirectory
expandUser ('~':'/':p) = do
    home <- getHomeDirectory
    return $ combine home p
expandUser p           = return p

