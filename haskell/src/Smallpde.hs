{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}
---fvectorise
module Smallpde where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Control.Monad
import Control.DeepSeq
import GHC.Prim

import Data.Array.Repa (ix2,fromFunction,DIM2,D,Array,(!)
                       , computeUnboxedP, computeUnboxedS,U(..),(+^),unsafeIndex)
import Data.Array.Repa.Index ((:.)(..),Z(..))
import Data.Array.Repa.Stencil  (Boundary(..))
import Data.Array.Repa.Stencil.Dim2  (stencil2,makeStencil2,mapStencil2)
import qualified Data.Array.Repa                          as A
import qualified Data.Array.Repa.Stencil                  as A
import qualified Data.Array.Repa.Stencil.Dim2             as A
import qualified Data.Array.Repa.Shape                    as S

import Control.Exception (bracket_)
import Debug.Trace (traceEventIO)

import Numeric

type RealT = Float

{-# INLINE event #-}
event :: String -> IO a -> IO a
event label =
  bracket_ (traceEventIO $ "START " ++ label)
           (traceEventIO $ "STOP "  ++ label)

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
        dirichlet' a = computeUnboxedSM $ mapStencil2 (BoundConst 0) stencil' a
        -- dirichlet' a = computeUnboxedSM $ A.szipWith (+) a $ A.smap (*d) $ mapStencil2 (BoundConst 0) stencil a

solve size iteration = do let f :: DIM2 -> RealT
                              f (Z :. x :. y) | x == y && x== size `div` 2 = 1
                                              | otherwise                  = 0
                              initialArray :: Array D DIM2 RealT
                              initialArray = fromFunction (ix2 size size) f
                              {-# INLINE computeUnboxedSM #-}
                              computeUnboxedSM !x = return (computeUnboxedS x)
                              
                          !initialArray' <- {-event "initial array" $-} computeUnboxedSM initialArray    
                          !array <- {-event "pde solve" $-} dirichlet iteration size initialArray'

                          return array

main = do let size      = 256-2
              iteration = 1024*5
              
          !array <- {-event "pde solve" $-} solve size iteration

          putStr   $! concat $! [ '0':" " | j <- take (size+2) $ [0..] ]
          putStrLn $! concat $! [ '\n' : (concat $! [ (showEFloat (Just 5) $ array `unsafeIndex` (ix2 i j)) " " | j <- take size $! [0..] ]) | i <- take size $ [0..] ]
          putStrLn $! concat $! [ '0':" " | j <- take (size+2) $ [0..] ]
