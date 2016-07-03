{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}
---fvectorise
module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Control.Monad
import Control.DeepSeq
import GHC.Prim

import Data.Array.Repa (ix2,fromFunction,DIM2,D,Array,(!)
                       , computeUnboxedP, computeUnboxedS,U(..),(+^))
import Data.Array.Repa.Index ((:.)(..),Z(..))
import Data.Array.Repa.Stencil  (Boundary(..))
import Data.Array.Repa.Stencil.Dim2  (stencil2,makeStencil2,mapStencil2)
import qualified Data.Array.Repa                          as A
import qualified Data.Array.Repa.Stencil                  as A
import qualified Data.Array.Repa.Stencil.Dim2             as A
import qualified Data.Array.Repa.Shape                    as S

import Numeric

type RealT = Float

dirichlet !steps !size !arr = go steps arr 
  where go  0 !arr = return arr
        go !n !arr = do arr' <- dirichlet' arr 
                        go (n-1) arr'
        !dx = 1/((fromInteger $ toInteger size)-1)
        !dt = dx*dx/4 / 2
        d :: RealT
        !d  = dt/dx/dx
        {-# INLINE stencil #-}
        !stencil  = [stencil2|  0  1  0
                               1  -4 1
                               0  1  0 |]
        {-# INLINE stencil' #-}
        !stencil' = makeStencil2 3 3 (\ix -> case ix of
                                                Z :. -1 :.  0  -> Just d
                                                Z :.  0 :. -1  -> Just d
                                                Z :.  0 :.  1  -> Just d
                                                Z :.  1 :.  0  -> Just d
                                                Z :.  0 :.  0  -> Just $! 1-4*d                                  
                                                _              -> Nothing)

        {-# INLINE computeUnboxedSM #-}
        computeUnboxedSM !x = return (computeUnboxedS x)
        {-# INLINE dirichlet' #-}                       
        dirichlet' :: Array U DIM2 RealT -> IO (Array U DIM2 RealT)
        -- dirichlet' a = computeUnboxedSM $ A.szipWith (+) a $ A.smap (*d) $ mapStencil2 (BoundConst 0) stencil a
        dirichlet' a = computeUnboxedSM $ mapStencil2 (BoundConst 0) stencil' a

main = do let size      = 256-2
              iteration = 1024*5
              f :: DIM2 -> RealT
              f (Z :. x :. y) | x == y && x== size `div` 2 = 1
                              | otherwise                  = 0
              initialArray :: Array D DIM2 RealT
              initialArray = fromFunction (ix2 size size) f
              {-# INLINE computeUnboxedSM #-}
              computeUnboxedSM !x = return (computeUnboxedS x)
              
          !initialArray' <- computeUnboxedSM initialArray    
          !array <- dirichlet iteration size initialArray'

          forM (take (size+1) $ [0..]) $ \j ->  
            putStr $ "0" ++ " "
          putStrLn "0"
          forM_ (take size $ [0..]) $ \i -> do
            putStr $ "0 "
            forM (take size $ [0..]) $ \j ->  
              putStr $ (showEFloat (Just 5) $ array ! (ix2 i j)) " "
            putStrLn "0"
          forM (take (size+1) $ [0..]) $ \j ->  
              putStr $ "0" ++ " "
          putStrLn "0"