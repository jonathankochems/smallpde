{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}
---fvectorise
module Main where

import qualified Smallpde

main = Smallpde.main
