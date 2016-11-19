{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, QuasiQuotes
           , ScopedTypeVariables, FlexibleContexts, GADTs, MagicHash,UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}

module SmallpdeSIMD where

import Prelude hiding (read,mapM_,mapM,foldr)
import Data.List (intercalate)
import qualified Data.List as List
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Generic.Mutable(new, read, write, unsafeRead, unsafeWrite, set)
import Control.Monad (forM, forM_)
import Control.Monad.Primitive (PrimState)
import Debug.Trace(traceShow)
import Numeric (showEFloat)
import qualified Data.Vector.Unboxed.SIMD as VUS
import qualified Data.Vector.Unboxed.SIMD.Internal as VUSI
import qualified Data.Vector.Primitive

import Data.Primitive.SIMD (unpackVector,packVector, FloatX4, unsafeInsertVector)

import Control.Monad

import Data.Primitive.SIMD (unpackVector,packVector, FloatX4, unsafeInsertVector)

import Language.Haskell.TH
import qualified Data.Vector.AcceleratedFor.Internal as AFI
import Data.Vector.AcceleratedFor.Internal (for1D,for2D,for2DSkip,for1D',for1D'',for2D'',for2D''',printQ,Accelerate(..))
import GHC.Prim
import Control.Monad.Primitive(primitive,primToIO,internal)
import GHC.Base (Int(..), Float (..))

import qualified Data.Primitive.ByteArray as ByteArray
import Data.ByteString.Char8(pack)
import Data.ByteString.Builder
import Data.Monoid
import System.IO(stdout)

import qualified Data.Primitive.ByteArray as ByteArray
import VectorInst
import SolverCodeGen

when True  m = m
when False _ = return ()

{-
    The Transformation:
    ====================
    The original grid:

      |                                |
    --------------------------------------
      |                                | 
      |                                |
    - |- - - - - - - - - - - - - - - - |-
      |                                |
      |                                |
    - |- - - - - - - - - - - - - - - - |-
      |                                |
      |                                |
    - |- - - - - - - - - - - - - - - - |-
      |                                |
      |                                |
    --------------------------------------
      |                                |
                                       
    The transformed grid:
         |                                                                                                                                 |
    --------------------------------------------------------------------------------------------------------------------------------------------
         |                                                                                                                                 |
         |                                                                                                                                 |
    --------------------------------------------------------------------------------------------------------------------------------------------
         |                                                                                                                                 |

    Within the boundary
    (i,j) -> (i% (n/4),4*j+i/(n/4))
    
-}

indexTransform !n !i !j  = (width*(i-sector*slice))+4*j+sector
    where !slice = (n+3) `div` 4
          !width = 4*n
          !sector = i `div` slice

indexTransformReverse width !i !j
       = n*i' + j'
    where !n             = width `div` 4
          !(!j',!sector) = j `divMod` 4
          !i'            = i+sector*n  

{-# INLINE solve #-}
solve :: Int -> Int -> IO (ByteArray.MutableByteArray (PrimState IO))
solve !n !iterations = 
        do let steps :: Int
               !steps = iterations
               d' :: Float 
               !d'  = dt/dx/dx
               d :: FloatX4 
               !d  = packVector (d',d',d',d')
               dt :: Float
               !dt = dx*dx/4/2
               dx :: Float
               !dx = 1/(n'*n'-1)
               n' :: Float
               !n' = fromInteger $ toInteger n
               !halfn = n `div` 2
               !nCeil = 4*((n+3) `div` 4)
           a <- ByteArray.newAlignedPinnedByteArray (n*n*4) 64
           ByteArray.setByteArray a 0 (n*n `div` 4) (0 :: FloatX4) 
           ByteArray.writeByteArray a (indexTransform n halfn halfn) (1.0 :: Float)
           b <- ByteArray.newAlignedPinnedByteArray (n*n*4) 64
           !() <- timeloop steps n d d' a b
           let arr | steps `mod` 2 == 0 = a
                   | otherwise          = b
           
           return arr
  where {-# INLINE timeloop #-}
        -- timeloop :: Int -> Int -> FloatX4 -> Float -> VU.MVector (PrimState IO) Float -> VU.MVector (PrimState IO) Float -> IO ()
        timeloop :: Int -> Int -> FloatX4 -> Float -> ByteArray.MutableByteArray (PrimState IO) -> ByteArray.MutableByteArray (PrimState IO) -> IO ()
        timeloop !steps !n !d !d' (ByteArray.MutableByteArray a#) (ByteArray.MutableByteArray b#) = do 
                              primitive $ go a# b#
         where !(I# n#)       = n 
               !(I# n'#)      = n `div` 4
               !(I# nborder#) = 4*n*(n `div` 4 - 1) 
               !(I# steps#) = steps
               !d#          = VUSI.coerceToFloatX4# d
               go ::MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> State# RealWorld -> (# State# RealWorld, () #)
               go rawa rawb s = $(AFI.generate internalSolve) 

printArray n a = go 0 0 (n-1) "" 
    where !slice = (n+3) `div` 4
          go !k !i !j !line | k >= 4 = do putStrLn ""
                                          putStrLn ""
                            | i >= n `div` 4 = go (k+1) 0 (n-1) ""
                            | j >= 0 = do !(num :: Float) <- ByteArray.readByteArray a $ 4*n*i+4*j+k
                                          go k i (j-1) $! (showEFloat (Just 5) num " "++line)
                            | j < 0  = do putStrLn line
                                          go k (i+1) (n-1) ""
    
printArrayRaw n a = do forM_ [0..n-1] (\(!i) -> do
                         --when (i `mod` slice == 0) $ putStrLn ""
                         !liness <- forM [0..n-1] (\(!j) -> do
                                      !num <- VGM.unsafeRead a $ n*i+j 
                                      --perf_marker
                                      return $! showEFloat (Just 5) num " ")
                         putStrLn $! concat liness)
                       putStrLn ""
                       putStrLn ""

printArrayhPutX4 :: Int -> ByteArray.MutableByteArray RealWorld -> IO ()
printArrayhPutX4 n (ByteArray.MutableByteArray a) = 
                             do builder <- build n a
                                hPutBuilder stdout builder
   where fastRead arr# (I# i#) = primitive go
            where go s = case readFloatArrayAsFloatX4# arr# i# s of
                                (# s', fs #) -> 
                                  case unpackFloatX4# fs of
                                  (# f1, f2, f3, f4 #) -> (# s', ( F# f1
                                                                 , F# f2
                                                                 , F# f3
                                                                 , F# f4
                                                                 ) #)
         build :: Int -> ByteArray.MutableByteArray# RealWorld -> IO (Builder)
         build n a = go 0 0 mempty mempty mempty mempty 
             where !slice = (n+3) `div` 4
                   go !i !j !b1 !b2 !b3 !b4 | j < 4*n   = do !(!f1',!f2',!f3',!f4') <- a `fastRead` (4*n*i+j)
                                                             let !f1 = string7 $ showEFloat (Just 5) f1' " "
                                                                 !f2 = string7 $ showEFloat (Just 5) f2' " "
                                                                 !f3 = string7 $ showEFloat (Just 5) f3' " "
                                                                 !f4 = string7 $ showEFloat (Just 5) f4' " "
                                                             go i (j+4) (b1 <> f1)
                                                                        (b2 <> f2)
                                                                        (b3 <> f3)
                                                                        (b4 <> f4)
                                            | i < slice-1 = go (i+1) 0 (b1 <> charUtf8 '\n')
                                                                       (b2 <> charUtf8 '\n')
                                                                       (b3 <> charUtf8 '\n')
                                                                       (b4 <> charUtf8 '\n')
                                            | otherwise = return $ b1 <> b2 <> b3 <> b4

main = do let n :: Int
              !n = 256
          !a <- solve n $ 5*1024
          printArrayhPutX4 n a
