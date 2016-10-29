{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, MagicHash, UnboxedTuples, 
             QuasiQuotes, ScopedTypeVariables, FlexibleContexts,GADTs #-}
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

import Language.Haskell.TH
import qualified Data.Vector.AcceleratedFor.Internal as AFI
import Data.Vector.AcceleratedFor.Internal (for1D,for2D,for2DSkip,for1D')
import GHC.Prim
import Control.Monad.Primitive(primitive,primToIO,internal)
import GHC.Base (Int(..))

main = defaultMain [
             bench "normalRead" $ nfIO (normalReadBench n)
           , bench "unsafeRead1" $ nfIO (unsafeReadBench n)
           , bench "unsafeRead8" $ nfIO (unsafeReadBench8 n)
           , bench "vectorisedRead" $ nfIO (vectorisedReadBench n)
           , bench "unsafeVectorisedRead" $ nfIO (unsafeVectorisedReadBench n)
           , bench "veryunsafeVectorisedRead" $ nfIO (veryunsafeVectorisedReadBench n)
           , bench "generatedVectorisedRead"  $ nfIO (generatedVectorisedReadBench n)
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

unsafeReadBench8 :: Int -> IO ()
unsafeReadBench8 n = do !floatMutableVector <- VU.thaw floatVector
                        forM_ [0..1000] $ \i -> do
                            forM_ [0..(n `div` 8)-1] $ \i -> do
                              f <- floatMutableVector `VGM.unsafeRead` (8*i) 
                              VGM.unsafeWrite floatMutableVector (8*i) (2*f)
                              f1 <- floatMutableVector `VGM.unsafeRead` (8*i+1) 
                              VGM.unsafeWrite floatMutableVector (8*i+1) (2*f1)
                              f2 <- floatMutableVector `VGM.unsafeRead` (8*i+2) 
                              VGM.unsafeWrite floatMutableVector (8*i+2) (2*f2)
                              f3 <- floatMutableVector `VGM.unsafeRead` (8*i+3) 
                              VGM.unsafeWrite floatMutableVector (8*i+3) (2*f3)
                              f4 <- floatMutableVector `VGM.unsafeRead` (8*i+4) 
                              VGM.unsafeWrite floatMutableVector (8*i+4) (2*f4)
                              f5 <- floatMutableVector `VGM.unsafeRead` (8*i+5) 
                              VGM.unsafeWrite floatMutableVector (8*i+5) (2*f5)
                              f6 <- floatMutableVector `VGM.unsafeRead` (8*i+6) 
                              VGM.unsafeWrite floatMutableVector (8*i+6) (2*f6)
                              f7 <- floatMutableVector `VGM.unsafeRead` (8*i+7) 
                              VGM.unsafeWrite floatMutableVector (8*i+7) (2*f7)
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

generatedVectorisedReadBench :: Int -> IO ()
generatedVectorisedReadBench n = do 
                              !floatMutableVector <- VU.thaw floatVector
                              let rawb = VUSI.convertToRawVector floatMutableVector
                              primitive $ go rawb
                                
    where     floatList :: [Float]
              !floatList = [0..fromInteger (toInteger $ n-1)]
              floatVector :: VU.Vector Float
              !floatVector = VU.fromList floatList
              !(I# n#) = n
              go ::MutableByteArray# RealWorld -> State# RealWorld -> (# State# RealWorld, () #)
              go rawb s = $(AFI.generate $ do 
                       let _rawb   = varE $ mkName "rawb"
                       for1D  [| 0# |] [| 1000# |] [| 1# |] $ \_ -> do
                         for1D' [| 0# |] [| n# |] [| 32# |] $ \_index -> do 
                             f  <- return <$> _rawb `AFI.readFloatArrayAsFloatQ` [| $(_index) |]
                             AFI.writeFloatArrayAsFloatQ _rawb [| $(_index) |] [| $(f) `plusFloatX4#` $(f)|] 
                             f' <- return <$> _rawb `AFI.readFloatArrayAsFloatQ` [| $(_index) +# 4#  |]
                             AFI.writeFloatArrayAsFloatQ _rawb [| $(_index) +# 4# |] [| $(f') `plusFloatX4#` $(f') |] 
                             f'' <- return <$> _rawb `AFI.readFloatArrayAsFloatQ` [| $(_index) +# 8#  |]
                             AFI.writeFloatArrayAsFloatQ _rawb [| $(_index) +# 8# |] [| $(f'') `plusFloatX4#` $(f'') |] 
                             f''' <- return <$> _rawb `AFI.readFloatArrayAsFloatQ` [| $(_index) +# 12#  |]
                             AFI.writeFloatArrayAsFloatQ _rawb [| $(_index) +# 12# |] [| $(f''') `plusFloatX4#` $(f''') |] 
                             f1  <- return <$> _rawb `AFI.readFloatArrayAsFloatQ` [| $(_index) +# 16# |]
                             AFI.writeFloatArrayAsFloatQ _rawb [| $(_index) +# 16# |] [| $(f1) `plusFloatX4#` $(f1)|] 
                             f1' <- return <$> _rawb `AFI.readFloatArrayAsFloatQ` [| $(_index) +# 20#  |]
                             AFI.writeFloatArrayAsFloatQ _rawb [| $(_index) +# 20# |] [| $(f1') `plusFloatX4#` $(f1') |] 
                             f1'' <- return <$> _rawb `AFI.readFloatArrayAsFloatQ` [| $(_index) +# 24#  |]
                             AFI.writeFloatArrayAsFloatQ _rawb [| $(_index) +# 24# |] [| $(f1'') `plusFloatX4#` $(f1'') |] 
                             f1''' <- return <$> _rawb `AFI.readFloatArrayAsFloatQ` [| $(_index) +# 28#  |]
                             AFI.writeFloatArrayAsFloatQ _rawb [| $(_index) +# 28# |] [| $(f1''') `plusFloatX4#` $(f1''') |] 
                             return [| () |])

                   
