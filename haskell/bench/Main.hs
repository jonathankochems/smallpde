{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}
---fvectorise
module Main where

--import qualified Smallpde
import qualified SmallpdeVector
import Criterion.Main

main = defaultMain [
             bench "fdm/1" $ nfIO (SmallpdeVector.main 1 n steps)
           , bench "fdm/2" $ nfIO (SmallpdeVector.main 2 n steps)
           , bench "fdm/4" $ nfIO (SmallpdeVector.main 4 n steps)
           , bench "fdm/8" $ nfIO (SmallpdeVector.main 8 n steps)
           , bench "fdm/16" $ nfIO (SmallpdeVector.main 16 n steps)
           , bench "fdm/32" $ nfIO (SmallpdeVector.main 32 n steps)
           , bench "fdm/64" $ nfIO (SmallpdeVector.main 64 n steps)
        ]
  where n     =  256
        steps =  5*1024
 
