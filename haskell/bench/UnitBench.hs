{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, QuasiQuotes, ScopedTypeVariables, FlexibleContexts,GADTs #-}
{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}
---fvectorise
module Main where

import Criterion.Main
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU

import qualified SmallpdeVector

import Control.Monad

import Data.Primitive.SIMD (unpackVector,packVector, FloatX4, unsafeInsertVector)
import qualified Data.Vector.Unboxed.SIMD as VUS
import qualified Data.Vector.Unboxed.SIMD.Internal as VUSI

main = defaultMain [
             bench "normalRead" $ nfIO (normalReadBench n)
           , bench "unsafeRead" $ nfIO (unsafeReadBench n)
           , bench "vectorisedRead" $ nfIO (vectorisedReadBench n)
           , bench "unsafeVectorisedRead" $ nfIO (unsafeVectorisedReadBench n)
           , bench "veryunsafeVectorisedRead" $ nfIO (veryunsafeVectorisedReadBench n)
--           , bench "rawVectorisedRead" $ nfIO (rawVectorisedReadBench n)
        ]
  where n     =  256
        steps =  5*1024

assert True  = return ()
assert False = error "assertion failure"

normalReadBench :: Int -> IO ()
normalReadBench n = do !floatMutableVector <- VU.thaw floatVector
                       forM_ [0..1000] $ \i -> do
                           forM_ [0..n-1] $ \i -> do
                             f <- floatMutableVector `VGM.read` i 
                             VGM.write floatMutableVector i (2*f)
    where     floatList :: [Float]
              !floatList = [0..fromInteger (toInteger $ n-1)]
              floatVector :: VU.Vector Float
              !floatVector = VU.fromList floatList


unsafeReadBench :: Int -> IO ()
unsafeReadBench n = do !floatMutableVector <- VU.thaw floatVector
                       forM_ [0..1000] $ \i -> do
                           forM_ [0..n-1] $ \i -> do
                             f <- floatMutableVector `VGM.unsafeRead` i 
                             VGM.unsafeWrite floatMutableVector i (2*f)
    where     floatList :: [Float]
              !floatList = [0..fromInteger (toInteger $ n-1)]
              floatVector :: VU.Vector Float
              !floatVector = VU.fromList floatList


vectorisedReadBench :: Int -> IO ()
vectorisedReadBench n = do !floatMutableVector <- VU.thaw floatVector
                           forM_ [0..1000] $ \i -> do
                               forM_ [0..(n `div` 4)-1] $ \i -> do
                                 f <- floatMutableVector `VUS.vectorisedRead` (i*4)
                                 VUS.vectorisedWrite floatMutableVector (4*i) (2*f) 
    where     floatList :: [Float]
              !floatList = [0..fromInteger (toInteger $ n-1)]
              floatVector :: VU.Vector Float
              !floatVector = VU.fromList floatList


unsafeVectorisedReadBench :: Int -> IO ()
unsafeVectorisedReadBench n = do !floatMutableVector <- VU.thaw floatVector
                                 forM_ [0..1000] $ \i -> do
                                     forM_ [0..(n `div` 4)-1] $ \i -> do
                                       f <- floatMutableVector `VUS.unsafeVectorisedRead` (i*4)
                                       VUS.unsafeVectorisedWrite floatMutableVector (4*i) (2*f) 
    where     floatList :: [Float]
              !floatList = [0..fromInteger (toInteger $ n-1)]
              floatVector :: VU.Vector Float
              !floatVector = VU.fromList floatList

veryunsafeVectorisedReadBench :: Int -> IO ()
veryunsafeVectorisedReadBench n = do !floatMutableVector <- VU.thaw floatVector
                                     forM_ [0..1000] $ \i -> do
                                         forM_ [0..(n `div` 4)-1] $ \i -> do
                                           f <- floatMutableVector `VUS.veryunsafeVectorisedRead` (i*4)
                                           VUS.veryunsafeVectorisedWrite floatMutableVector (4*i) (2*f) 
    where     floatList :: [Float]
              !floatList = [0..fromInteger (toInteger $ n-1)]
              floatVector :: VU.Vector Float
              !floatVector = VU.fromList floatList

--rawVectorisedReadBench :: Int -> IO ()
--rawVectorisedReadBench n = do !floatMutableVector <- VU.thaw floatVector
--                              let rawb = VUSI.convertToRawVector floatMutableVector
--                              forM_ [0..1000] $ \i -> do
--                                  forM_ [0..(n `div` 4)-1] $ \i -> do
--                                    rawb `VUSI.rawVectorisedRead'` (i*4)
--    where     floatList :: [Float]
--              !floatList = [0..fromInteger (toInteger $ n-1)]
--              floatVector :: VU.Vector Float
--              !floatVector = VU.fromList floatList

                   
