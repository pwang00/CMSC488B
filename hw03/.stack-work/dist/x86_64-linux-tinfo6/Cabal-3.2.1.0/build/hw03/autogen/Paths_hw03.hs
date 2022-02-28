{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw03 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/philip2000/Documents/CMSC488B/hw03/.stack-work/install/x86_64-linux-tinfo6/1137d01a946e65a5abdbd5f3c13610f4a29714e0cb33c72037754e8d38d653e9/8.10.4/bin"
libdir     = "/home/philip2000/Documents/CMSC488B/hw03/.stack-work/install/x86_64-linux-tinfo6/1137d01a946e65a5abdbd5f3c13610f4a29714e0cb33c72037754e8d38d653e9/8.10.4/lib/x86_64-linux-ghc-8.10.4/hw03-0.1.0.0-DDD6Y2hmQY98GDdsxaIOx4-hw03"
dynlibdir  = "/home/philip2000/Documents/CMSC488B/hw03/.stack-work/install/x86_64-linux-tinfo6/1137d01a946e65a5abdbd5f3c13610f4a29714e0cb33c72037754e8d38d653e9/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/philip2000/Documents/CMSC488B/hw03/.stack-work/install/x86_64-linux-tinfo6/1137d01a946e65a5abdbd5f3c13610f4a29714e0cb33c72037754e8d38d653e9/8.10.4/share/x86_64-linux-ghc-8.10.4/hw03-0.1.0.0"
libexecdir = "/home/philip2000/Documents/CMSC488B/hw03/.stack-work/install/x86_64-linux-tinfo6/1137d01a946e65a5abdbd5f3c13610f4a29714e0cb33c72037754e8d38d653e9/8.10.4/libexec/x86_64-linux-ghc-8.10.4/hw03-0.1.0.0"
sysconfdir = "/home/philip2000/Documents/CMSC488B/hw03/.stack-work/install/x86_64-linux-tinfo6/1137d01a946e65a5abdbd5f3c13610f4a29714e0cb33c72037754e8d38d653e9/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw03_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw03_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw03_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw03_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw03_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw03_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
