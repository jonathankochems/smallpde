{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}
---fvectorise
module Main where

--import qualified Smallpde
import qualified SmallpdeVector

main = SmallpdeVector.main
