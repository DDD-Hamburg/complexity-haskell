module Paths_complexity_in_haskell (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/virgiliolino/haskell/complexity-in-haskell/.stack-work/install/x86_64-linux/lts-3.1/7.10.2/bin"
libdir     = "/home/virgiliolino/haskell/complexity-in-haskell/.stack-work/install/x86_64-linux/lts-3.1/7.10.2/lib/x86_64-linux-ghc-7.10.2/complexity-in-haskell-0.1.0.0-7Byd1J3oc83A4nThTTjwsS"
datadir    = "/home/virgiliolino/haskell/complexity-in-haskell/.stack-work/install/x86_64-linux/lts-3.1/7.10.2/share/x86_64-linux-ghc-7.10.2/complexity-in-haskell-0.1.0.0"
libexecdir = "/home/virgiliolino/haskell/complexity-in-haskell/.stack-work/install/x86_64-linux/lts-3.1/7.10.2/libexec"
sysconfdir = "/home/virgiliolino/haskell/complexity-in-haskell/.stack-work/install/x86_64-linux/lts-3.1/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "complexity_in_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "complexity_in_haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "complexity_in_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "complexity_in_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "complexity_in_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
