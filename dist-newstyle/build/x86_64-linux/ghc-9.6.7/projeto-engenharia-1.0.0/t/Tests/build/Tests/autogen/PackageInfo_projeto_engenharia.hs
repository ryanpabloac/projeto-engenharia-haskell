{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_projeto_engenharia (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "projeto_engenharia"
version :: Version
version = Version [1,0,0] []

synopsis :: String
synopsis = "Projeto de Engenharia em Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
