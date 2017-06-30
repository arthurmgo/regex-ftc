{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_regex_ftc (
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

bindir     = "C:\\Users\\arthu\\Documents\\GitHub\\regex-ftc\\.stack-work\\install\\e77882c1\\bin"
libdir     = "C:\\Users\\arthu\\Documents\\GitHub\\regex-ftc\\.stack-work\\install\\e77882c1\\lib\\x86_64-windows-ghc-8.0.2\\regex-ftc-0.1.0.0-4o6NQxocRYX9a1nXhmykiN"
dynlibdir  = "C:\\Users\\arthu\\Documents\\GitHub\\regex-ftc\\.stack-work\\install\\e77882c1\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\arthu\\Documents\\GitHub\\regex-ftc\\.stack-work\\install\\e77882c1\\share\\x86_64-windows-ghc-8.0.2\\regex-ftc-0.1.0.0"
libexecdir = "C:\\Users\\arthu\\Documents\\GitHub\\regex-ftc\\.stack-work\\install\\e77882c1\\libexec"
sysconfdir = "C:\\Users\\arthu\\Documents\\GitHub\\regex-ftc\\.stack-work\\install\\e77882c1\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "regex_ftc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "regex_ftc_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "regex_ftc_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "regex_ftc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "regex_ftc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "regex_ftc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
