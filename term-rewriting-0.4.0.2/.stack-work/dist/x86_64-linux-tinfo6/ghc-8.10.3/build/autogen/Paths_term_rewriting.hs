{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_term_rewriting (
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
version = Version [0,4,0,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/pdabl/bachelorarbeit/ttt_goes_liquid_haskell/term-rewriting-0.4.0.2/.stack-work/install/x86_64-linux-tinfo6/fdfe8a415ba71a01ea43b8c484637796f8152aed18fcc1035ce2aa8f2947d575/8.10.3/bin"
libdir     = "/home/pdabl/bachelorarbeit/ttt_goes_liquid_haskell/term-rewriting-0.4.0.2/.stack-work/install/x86_64-linux-tinfo6/fdfe8a415ba71a01ea43b8c484637796f8152aed18fcc1035ce2aa8f2947d575/8.10.3/lib/x86_64-linux-ghc-8.10.3/term-rewriting-0.4.0.2-1qVx0sebI5J6AlFbYHVIc"
dynlibdir  = "/home/pdabl/bachelorarbeit/ttt_goes_liquid_haskell/term-rewriting-0.4.0.2/.stack-work/install/x86_64-linux-tinfo6/fdfe8a415ba71a01ea43b8c484637796f8152aed18fcc1035ce2aa8f2947d575/8.10.3/lib/x86_64-linux-ghc-8.10.3"
datadir    = "/home/pdabl/bachelorarbeit/ttt_goes_liquid_haskell/term-rewriting-0.4.0.2/.stack-work/install/x86_64-linux-tinfo6/fdfe8a415ba71a01ea43b8c484637796f8152aed18fcc1035ce2aa8f2947d575/8.10.3/share/x86_64-linux-ghc-8.10.3/term-rewriting-0.4.0.2"
libexecdir = "/home/pdabl/bachelorarbeit/ttt_goes_liquid_haskell/term-rewriting-0.4.0.2/.stack-work/install/x86_64-linux-tinfo6/fdfe8a415ba71a01ea43b8c484637796f8152aed18fcc1035ce2aa8f2947d575/8.10.3/libexec/x86_64-linux-ghc-8.10.3/term-rewriting-0.4.0.2"
sysconfdir = "/home/pdabl/bachelorarbeit/ttt_goes_liquid_haskell/term-rewriting-0.4.0.2/.stack-work/install/x86_64-linux-tinfo6/fdfe8a415ba71a01ea43b8c484637796f8152aed18fcc1035ce2aa8f2947d575/8.10.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "term_rewriting_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "term_rewriting_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "term_rewriting_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "term_rewriting_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "term_rewriting_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "term_rewriting_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
