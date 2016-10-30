{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, MagicHash, UnboxedTuples, 
             QuasiQuotes, ScopedTypeVariables, FlexibleContexts,GADTs #-}
{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}
---fvectorise
module StencilBench where

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
import Data.Vector.AcceleratedFor.Internal (for1D,for2D,for2DSkip,for1D',for1D'',for2D'')
import GHC.Prim
import Control.Monad.Primitive(primitive,primToIO,internal)
import GHC.Base (Int(..))

assert True  = return ()
assert False = error "assertion failure"


stencilBench :: Int -> Int -> IO ()
stencilBench steps size = 
                    do !floatMutableVector <- VU.thaw floatVector
                       forM_ [0..steps] $ \_ -> do
                         forM_ [1..n-2] $ \i -> do
                           forM_ [1..n-2] $ \j -> do
                             north <- floatMutableVector `VGM.unsafeRead` (n*(i-1) + j )
                             east  <- floatMutableVector `VGM.unsafeRead` (n*i + j - 1 )
                             here  <- floatMutableVector `VGM.unsafeRead` (n*i + j     )
                             west  <- floatMutableVector `VGM.unsafeRead` (n*i + j + 1 )
                             south <- floatMutableVector `VGM.unsafeRead` (n*(i+1) + j )
                             VGM.unsafeWrite floatMutableVector i ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
    where floatList :: [Float]
          !floatList = [0..fromInteger (toInteger $ size-1)]
          floatVector :: VU.Vector Float
          !floatVector = VU.fromList floatList

          n :: Int
          n = toInt . sqrt $ fromIntegral size
          d = 0.5

toInt :: RealFrac a => a -> Int
toInt = fromIntegral . floor
-- unsafeReadBench8 :: Int -> Int -> IO ()
-- unsafeReadBench8 steps n = 
--                      do !floatMutableVector <- VU.thaw floatVector
--                         forM_ [0..steps] $ \i -> do
--                             forM_ [0..(n `div` 8)-1] $ \i -> do
--                               f <- floatMutableVector `VGM.unsafeRead` (8*i) 
--                               VGM.unsafeWrite floatMutableVector (8*i) (2*f)
--                               f1 <- floatMutableVector `VGM.unsafeRead` (8*i+1) 
--                               VGM.unsafeWrite floatMutableVector (8*i+1) (2*f1)
--                               f2 <- floatMutableVector `VGM.unsafeRead` (8*i+2) 
--                               VGM.unsafeWrite floatMutableVector (8*i+2) (2*f2)
--                               f3 <- floatMutableVector `VGM.unsafeRead` (8*i+3) 
--                               VGM.unsafeWrite floatMutableVector (8*i+3) (2*f3)
--                               f4 <- floatMutableVector `VGM.unsafeRead` (8*i+4) 
--                               VGM.unsafeWrite floatMutableVector (8*i+4) (2*f4)
--                               f5 <- floatMutableVector `VGM.unsafeRead` (8*i+5) 
--                               VGM.unsafeWrite floatMutableVector (8*i+5) (2*f5)
--                               f6 <- floatMutableVector `VGM.unsafeRead` (8*i+6) 
--                               VGM.unsafeWrite floatMutableVector (8*i+6) (2*f6)
--                               f7 <- floatMutableVector `VGM.unsafeRead` (8*i+7) 
--                               VGM.unsafeWrite floatMutableVector (8*i+7) (2*f7)
--     where     floatList :: [Float]
--               !floatList = [0..fromInteger (toInteger $ n-1)]
--               floatVector :: VU.Vector Float
--               !floatVector = VU.fromList floatList

generatedStencilBench :: Int -> Int -> IO ()
generatedStencilBench steps size = do 
                              !floatMutableVector <- VU.thaw floatVector
                              let rawb = VUSI.convertToRawVector floatMutableVector
                              primitive $ go rawb
                                
    where     floatList :: [Float]
              !floatList = [0..fromInteger (toInteger $ size-1)]
              floatVector :: VU.Vector Float
              !floatVector = VU.fromList floatList
              !(I# n#)     = (toInt . sqrt $ fromIntegral size)
              !(I# n'#)    = (toInt . sqrt $ fromIntegral size) `div` 4
              !(I# steps#) = steps
              d            = broadcastFloatX4# 0.5#
              go ::MutableByteArray# RealWorld -> State# RealWorld -> (# State# RealWorld, () #)
              go rawb s = $(AFI.generate $ do 
                       let _rawb   = varE $ mkName "rawb"
                           _d      = varE $ mkName "d"
                       for2D''  [| 0# |] [| steps# |] [| 1# |] 
                                [| 0# |] [| n'# |]    [| 1# |]  
                                [| 0# |] [| n# |]     [| 4# |] $ \_ -> \_i -> \_j -> do 
                             north  <- return <$> _rawb `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) |]
                             east   <- return <$> _rawb `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) -# 4#   |]
                             here   <- return <$> _rawb `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j)         |]
                             west   <- return <$> _rawb `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 4#   |]
                             south  <- return <$> _rawb `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) +# 1#) +# $(_j) |]
                             AFI.writeFloatArrayAsFloatQ _rawb [| 4# *# n# *# $(_i) +# $(_j) |]  
                                                 [| (( (broadcastFloatX4# 1.0#) `minusFloatX4#` $(_d)) `timesFloatX4#` $(here))  
                                                    `plusFloatX4#` 
                                                    ($(_d)  `timesFloatX4#` (  $(north)
                                                              `plusFloatX4#`  $(east)
                                                              `plusFloatX4#`  $(west)
                                                              `plusFloatX4#`  $(south)
                                                                           )
                                                    ) |] 
                             return [| () |])

                   
