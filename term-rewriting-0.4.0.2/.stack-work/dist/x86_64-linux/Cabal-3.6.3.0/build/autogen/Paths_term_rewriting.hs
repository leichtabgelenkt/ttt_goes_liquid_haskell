{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_term_rewriting (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/luca/uni/ttt_goes_liquid_haskell/term-rewriting-0.4.0.2/.stack-work/install/x86_64-linux/129431345b770cb684b3ecf32c4675d19e299d5fb0b0fd948001a71831910e3b/9.2.7/bin"
libdir     = "/home/luca/uni/ttt_goes_liquid_haskell/term-rewriting-0.4.0.2/.stack-work/install/x86_64-linux/129431345b770cb684b3ecf32c4675d19e299d5fb0b0fd948001a71831910e3b/9.2.7/lib/x86_64-linux-ghc-9.2.7/term-rewriting-0.4.0.2-3vOp25qbhQ17K5tDXBh7Vg"
dynlibdir  = "/home/luca/uni/ttt_goes_liquid_haskell/term-rewriting-0.4.0.2/.stack-work/install/x86_64-linux/129431345b770cb684b3ecf32c4675d19e299d5fb0b0fd948001a71831910e3b/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/home/luca/uni/ttt_goes_liquid_haskell/term-rewriting-0.4.0.2/.stack-work/install/x86_64-linux/129431345b770cb684b3ecf32c4675d19e299d5fb0b0fd948001a71831910e3b/9.2.7/share/x86_64-linux-ghc-9.2.7/term-rewriting-0.4.0.2"
libexecdir = "/home/luca/uni/ttt_goes_liquid_haskell/term-rewriting-0.4.0.2/.stack-work/install/x86_64-linux/129431345b770cb684b3ecf32c4675d19e299d5fb0b0fd948001a71831910e3b/9.2.7/libexec/x86_64-linux-ghc-9.2.7/term-rewriting-0.4.0.2"
sysconfdir = "/home/luca/uni/ttt_goes_liquid_haskell/term-rewriting-0.4.0.2/.stack-work/install/x86_64-linux/129431345b770cb684b3ecf32c4675d19e299d5fb0b0fd948001a71831910e3b/9.2.7/etc"

getBinDir     = catchIO (getEnv "term_rewriting_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "term_rewriting_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "term_rewriting_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "term_rewriting_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "term_rewriting_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "term_rewriting_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'