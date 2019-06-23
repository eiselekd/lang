{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_eval (
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
version = Version [0,0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/eiselekd/.cabal/bin"
libdir     = "/home/eiselekd/.cabal/lib/x86_64-linux-ghc-8.2.2/eval-0.0.0.1-8pGJd1HZMbv83nucdwXeXU-test"
dynlibdir  = "/home/eiselekd/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/eiselekd/.cabal/share/x86_64-linux-ghc-8.2.2/eval-0.0.0.1"
libexecdir = "/home/eiselekd/.cabal/libexec/x86_64-linux-ghc-8.2.2/eval-0.0.0.1"
sysconfdir = "/home/eiselekd/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "eval_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "eval_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "eval_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "eval_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "eval_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "eval_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
