{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_typeLevelProgramming (
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

bindir     = "/Users/alokpandey/Haskell/Type Level Programming/typeLevelProgramming/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/bin"
libdir     = "/Users/alokpandey/Haskell/Type Level Programming/typeLevelProgramming/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/lib/x86_64-osx-ghc-8.4.3/typeLevelProgramming-0.1.0.0-J4CTAQHxllb28ApTmyIIYP-typeLevelProgramming"
dynlibdir  = "/Users/alokpandey/Haskell/Type Level Programming/typeLevelProgramming/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/alokpandey/Haskell/Type Level Programming/typeLevelProgramming/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/share/x86_64-osx-ghc-8.4.3/typeLevelProgramming-0.1.0.0"
libexecdir = "/Users/alokpandey/Haskell/Type Level Programming/typeLevelProgramming/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/libexec/x86_64-osx-ghc-8.4.3/typeLevelProgramming-0.1.0.0"
sysconfdir = "/Users/alokpandey/Haskell/Type Level Programming/typeLevelProgramming/.stack-work/install/x86_64-osx/lts-12.13/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "typeLevelProgramming_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "typeLevelProgramming_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "typeLevelProgramming_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "typeLevelProgramming_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "typeLevelProgramming_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "typeLevelProgramming_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
