{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw01 (
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

bindir     = "/home/pwang00/Documents/CMSC488B/hw01/.stack-work/install/x86_64-linux-tinfo6/6a63ef6c5f5a5daa614f9fa05e0c047c2e1ede65aa0ee173a41afd088ad57ac8/8.10.4/bin"
libdir     = "/home/pwang00/Documents/CMSC488B/hw01/.stack-work/install/x86_64-linux-tinfo6/6a63ef6c5f5a5daa614f9fa05e0c047c2e1ede65aa0ee173a41afd088ad57ac8/8.10.4/lib/x86_64-linux-ghc-8.10.4/hw01-0.1.0.0-3VGRluXCyRyK8yOu8cppif-hw01"
dynlibdir  = "/home/pwang00/Documents/CMSC488B/hw01/.stack-work/install/x86_64-linux-tinfo6/6a63ef6c5f5a5daa614f9fa05e0c047c2e1ede65aa0ee173a41afd088ad57ac8/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/pwang00/Documents/CMSC488B/hw01/.stack-work/install/x86_64-linux-tinfo6/6a63ef6c5f5a5daa614f9fa05e0c047c2e1ede65aa0ee173a41afd088ad57ac8/8.10.4/share/x86_64-linux-ghc-8.10.4/hw01-0.1.0.0"
libexecdir = "/home/pwang00/Documents/CMSC488B/hw01/.stack-work/install/x86_64-linux-tinfo6/6a63ef6c5f5a5daa614f9fa05e0c047c2e1ede65aa0ee173a41afd088ad57ac8/8.10.4/libexec/x86_64-linux-ghc-8.10.4/hw01-0.1.0.0"
sysconfdir = "/home/pwang00/Documents/CMSC488B/hw01/.stack-work/install/x86_64-linux-tinfo6/6a63ef6c5f5a5daa614f9fa05e0c047c2e1ede65aa0ee173a41afd088ad57ac8/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw01_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw01_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw01_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw01_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw01_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw01_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
