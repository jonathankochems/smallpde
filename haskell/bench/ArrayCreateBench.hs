{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, MagicHash, UnboxedTuples, 
             QuasiQuotes, ScopedTypeVariables, FlexibleContexts,GADTs #-}
{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}
module ArrayCreateBench where

import Criterion.Main
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU

import qualified SmallpdeVector

import Control.Monad

import Data.Primitive.SIMD (unpackVector,packVector, FloatX4, unsafeInsertVector)
import qualified Data.Vector.Unboxed.SIMD as VUS
import qualified Data.Vector.Unboxed.SIMD.Internal as VUSI

import Language.Haskell.TH
import qualified Data.Vector.AcceleratedFor.Internal as AFI
import Data.Vector.AcceleratedFor.Internal (for1D,for2D,for2DSkip,for1D',for1D'',for2D'',for2D''')
import GHC.Prim
import Control.Monad.Primitive(primitive,primToIO,internal)
import GHC.Base (Int(..))

import SolverCodeGen

import qualified Data.Primitive.ByteArray as ByteArray

assert :: Bool -> IO ()
assert True  = return ()
assert False = error "assertion failure"

createArrayBench :: Int -> IO ()
createArrayBench size = 
                    do let a'  :: VU.Vector Float 
                           !a' = VU.create (do !a <- VGM.new (size*size); VGM.set a 0; return a)
                       !a <- VU.thaw a'
                       assert $ (a' VU.! mid) == 0
   where !mid = size*size `div` 4  

createArrayUnsafeBench :: Int -> IO ()
createArrayUnsafeBench size = 
                    do let a'  :: VU.Vector Float 
                           !a' = VU.create (do !a <- VGM.unsafeNew (size*size); VGM.set a 0; return a)
                       !a <- VU.unsafeThaw a'
                       assert $ (a' VU.! mid) == 0
   where !mid = size*size `div` 4  

createByteArrayBench :: Int -> IO ()
createByteArrayBench size = 
                    do (a :: ByteArray.MutableByteArray RealWorld) <- ByteArray.newAlignedPinnedByteArray (size*size*4) 64
                       (x :: Float) <- a `ByteArray.readByteArray` mid
                       assert $ x == 0
   where !mid = size*size `div` 4  
