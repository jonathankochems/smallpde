{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}
---fvectorise
module Main where

import qualified SmallpdeVector
import qualified SmallpdeSIMD

main = SmallpdeSIMD.main
