{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, MagicHash, UnboxedTuples, 
             QuasiQuotes, ScopedTypeVariables, FlexibleContexts,GADTs #-}
{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}
module PrintArrayBench where

import Criterion.Main
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU

import qualified SmallpdeVector

import Control.Monad

import Data.Primitive.SIMD (unpackVector,packVector, FloatX4, unsafeInsertVector)
--import qualified Data.Vector.Unboxed.SIMD as VUS
--import qualified Data.Vector.Unboxed.SIMD.Internal as VUSI

import Language.Haskell.TH
import qualified Data.Vector.AcceleratedFor.Internal as AFI
import Data.Vector.AcceleratedFor.Internal (for1D,for2D,for2DSkip,for1D',for1D'',for2D'',for2D''')
import GHC.Prim
import Control.Monad.Primitive(primitive,primToIO,internal)
import GHC.Base (Int(..),Float(..))

import qualified Data.Vector.Unboxed.SIMD.Internal as VUSI

import SolverCodeGen
import Numeric (showEFloat)

import qualified Data.Primitive.ByteArray as ByteArray
import Data.ByteString.Char8(pack)
import Data.ByteString.Builder
import Data.Monoid
import System.IO(stdout)

assert :: Bool -> IO ()
assert True  = return ()
assert False = error "assertion failure"

printArrayBench :: Int -> VU.MVector RealWorld Float -> IO ()
printArrayBench size floatMutableVector = do printArray n floatMutableVector 
   where  n = ceiling . sqrt . fromInteger $ toInteger size 
          printArray :: Int -> VU.MVector RealWorld Float -> IO ()
          printArray n a = go 0 0 (n-1) "" 
             where !slice = (n+3) `div` 4
                   go !k !i !j !line | k >= 4 = do putStrLn ""
                                                   putStrLn ""
                                     | i >= n `div` 4 = go (k+1) 0 (n-1) ""
                                     | j >= 0 = do !(num :: Float) <- VGM.unsafeRead a $ 4*n*i+4*j+k
                                                   go k i (j-1) $! (showEFloat (Just 5) num " "++line)
                                     | j < 0  = do putStrLn line
                                                   go k (i+1) (n-1) ""

printArrayRawBench :: Int -> VU.MVector RealWorld Float -> IO ()
printArrayRawBench size floatMutableVector = do printArrayRaw n floatMutableVector 
   where n = ceiling . sqrt . fromInteger $ toInteger size
         printArrayRaw :: Int -> VU.MVector RealWorld Float -> IO ()
         printArrayRaw n a = do forM_ [0..n-1] (\(!i) -> do
                                  !liness <- forM [0..n-1] (\(!j) -> do
                                               !num <- VGM.unsafeRead a $ n*i+j 
                                               return $! showEFloat (Just 5) num " ")
                                  putStrLn $! concat liness)
                                putStrLn ""
                                putStrLn ""

printArrayhPutBench :: Int -> VU.MVector RealWorld Float -> IO ()
printArrayhPutBench size floatMutableVector = do VU.unsafeFreeze floatMutableVector >>= printArrayRaw n  
   where printArrayRaw :: Int -> VU.Vector Float -> IO ()
         printArrayRaw n a = hPutBuilder stdout $ build n a
         n = ceiling . sqrt . fromInteger $ toInteger size
         build n a = snd $ VU.foldr add_line (0,mempty) a
            where add_line f (!i,!s) | i + 1 == n = (0,   ( byteString . pack $ showEFloat (Just 5) f " ") <> charUtf8 '\n' <> s)
                                     | otherwise  = (i+1, ( byteString . pack $ showEFloat (Just 5) f " ") <> s)

printArrayhPutX4Bench :: Int -> VU.MVector RealWorld Float -> IO ()
printArrayhPutX4Bench size floatMutableVector = printArrayRaw n (VUSI.convertToRawVector floatMutableVector)
   where printArrayRaw :: Int -> ByteArray.MutableByteArray# RealWorld -> IO ()
         printArrayRaw n a = do builder <- build n a
                                hPutBuilder stdout builder
         n = ceiling . sqrt . fromInteger $ toInteger size
         fastRead arr# (I# i#) = primitive go
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
                                            | i < slice = go (i+1) 0 (b1 <> charUtf8 '\n')
                                                                          (b2 <> charUtf8 '\n')
                                                                          (b3 <> charUtf8 '\n')
                                                                          (b4 <> charUtf8 '\n')
                                            | otherwise = return $ b1 <> b2 <> b3 <> b4
         
