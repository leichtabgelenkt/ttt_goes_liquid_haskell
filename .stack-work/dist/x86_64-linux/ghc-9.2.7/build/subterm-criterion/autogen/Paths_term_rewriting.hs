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
version = Version [1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/pdabl/bachelorarbeit/ttt_goes_liquid_haskell/.stack-work/install/x86_64-linux/52289139e3d7fc67154ff11cfb4aa5baeb8d9ab383f9b531740e3a3d1cf1e4c9/9.2.7/bin"
libdir     = "/home/pdabl/bachelorarbeit/ttt_goes_liquid_haskell/.stack-work/install/x86_64-linux/52289139e3d7fc67154ff11cfb4aa5baeb8d9ab383f9b531740e3a3d1cf1e4c9/9.2.7/lib/x86_64-linux-ghc-9.2.7/term-rewriting-1.0-8HgAJOyNCr4Ka72JbKHsHS-subterm-criterion"
dynlibdir  = "/home/pdabl/bachelorarbeit/ttt_goes_liquid_haskell/.stack-work/install/x86_64-linux/52289139e3d7fc67154ff11cfb4aa5baeb8d9ab383f9b531740e3a3d1cf1e4c9/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/home/pdabl/bachelorarbeit/ttt_goes_liquid_haskell/.stack-work/install/x86_64-linux/52289139e3d7fc67154ff11cfb4aa5baeb8d9ab383f9b531740e3a3d1cf1e4c9/9.2.7/share/x86_64-linux-ghc-9.2.7/term-rewriting-1.0"
libexecdir = "/home/pdabl/bachelorarbeit/ttt_goes_liquid_haskell/.stack-work/install/x86_64-linux/52289139e3d7fc67154ff11cfb4aa5baeb8d9ab383f9b531740e3a3d1cf1e4c9/9.2.7/libexec/x86_64-linux-ghc-9.2.7/term-rewriting-1.0"
sysconfdir = "/home/pdabl/bachelorarbeit/ttt_goes_liquid_haskell/.stack-work/install/x86_64-linux/52289139e3d7fc67154ff11cfb4aa5baeb8d9ab383f9b531740e3a3d1cf1e4c9/9.2.7/etc"

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
