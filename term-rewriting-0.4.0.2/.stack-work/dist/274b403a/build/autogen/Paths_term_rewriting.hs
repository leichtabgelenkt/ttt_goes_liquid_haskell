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

bindir     = "C:\\Users\\pdabl\\OneDrive\\Dokumente\\Studium Informatik\\Bachelorarbeit\\ttt_goes_liquid_haskell\\term-rewriting-0.4.0.2\\.stack-work\\install\\50918f85\\bin"
libdir     = "C:\\Users\\pdabl\\OneDrive\\Dokumente\\Studium Informatik\\Bachelorarbeit\\ttt_goes_liquid_haskell\\term-rewriting-0.4.0.2\\.stack-work\\install\\50918f85\\lib\\x86_64-windows-ghc-8.10.3\\term-rewriting-0.4.0.2-CWOCtxd26jXCUqEsqAs3Xb"
dynlibdir  = "C:\\Users\\pdabl\\OneDrive\\Dokumente\\Studium Informatik\\Bachelorarbeit\\ttt_goes_liquid_haskell\\term-rewriting-0.4.0.2\\.stack-work\\install\\50918f85\\lib\\x86_64-windows-ghc-8.10.3"
datadir    = "C:\\Users\\pdabl\\OneDrive\\Dokumente\\Studium Informatik\\Bachelorarbeit\\ttt_goes_liquid_haskell\\term-rewriting-0.4.0.2\\.stack-work\\install\\50918f85\\share\\x86_64-windows-ghc-8.10.3\\term-rewriting-0.4.0.2"
libexecdir = "C:\\Users\\pdabl\\OneDrive\\Dokumente\\Studium Informatik\\Bachelorarbeit\\ttt_goes_liquid_haskell\\term-rewriting-0.4.0.2\\.stack-work\\install\\50918f85\\libexec\\x86_64-windows-ghc-8.10.3\\term-rewriting-0.4.0.2"
sysconfdir = "C:\\Users\\pdabl\\OneDrive\\Dokumente\\Studium Informatik\\Bachelorarbeit\\ttt_goes_liquid_haskell\\term-rewriting-0.4.0.2\\.stack-work\\install\\50918f85\\etc"

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
  return (dir ++ "\\" ++ name)
