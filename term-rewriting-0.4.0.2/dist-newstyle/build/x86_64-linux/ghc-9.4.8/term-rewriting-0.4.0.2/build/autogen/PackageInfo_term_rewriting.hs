{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_term_rewriting (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "term_rewriting"
version :: Version
version = Version [0,4,0,2] []

synopsis :: String
synopsis = "Term Rewriting Library"
copyright :: String
copyright = ""
homepage :: String
homepage = "http://cl-informatik.uibk.ac.at/software/haskell-rewriting/"
