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
import Data.Vector.AcceleratedFor.Internal (for1D,for2D,for2DSkip,for1D',for1D'',for2D'',for2D''')
import GHC.Prim
import Control.Monad.Primitive(primitive,primToIO,internal)
import GHC.Base (Int(..))

import SolverCodeGen

import qualified Data.Primitive.ByteArray as ByteArray

assert True  = return ()
assert False = error "assertion failure"


stencilBench :: Int -> Int -> IO ()
stencilBench steps size = 
                    do !floatMutableVector  <- VU.thaw floatVector
                       !floatMutableVector' <- VU.thaw floatVector'
                       forM_ [0..steps `div` 2] $ \_ -> do
                         forM_ [1..n-2] $ \i -> do
                           forM_ [1..n-2] $ \j -> do
                             north <- floatMutableVector `VGM.unsafeRead` (n*(i-1) + j )
                             east  <- floatMutableVector `VGM.unsafeRead` (n*i + j - 1 )
                             here  <- floatMutableVector `VGM.unsafeRead` (n*i + j     )
                             west  <- floatMutableVector `VGM.unsafeRead` (n*i + j + 1 )
                             south <- floatMutableVector `VGM.unsafeRead` (n*(i+1) + j )
                             VGM.unsafeWrite floatMutableVector' (n*i + j) 
                                                           ((1-4*d) * here  
                                                                    + d*(   north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                         forM_ [1..n-2] $ \i -> do
                           forM_ [1..n-2] $ \j -> do
                             north <- floatMutableVector' `VGM.unsafeRead` (n*(i-1) + j )
                             east  <- floatMutableVector' `VGM.unsafeRead` (n*i + j - 1 )
                             here  <- floatMutableVector' `VGM.unsafeRead` (n*i + j     )
                             west  <- floatMutableVector' `VGM.unsafeRead` (n*i + j + 1 )
                             south <- floatMutableVector' `VGM.unsafeRead` (n*(i+1) + j )
                             VGM.unsafeWrite floatMutableVector (n*i + j) 
                                                           ((1-4*d) * here  
                                                                    + d*(   north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
    where floatList :: [Float]
          !floatList = [0..fromInteger (toInteger $ size-1)]
          floatList' :: [Float]
          !floatList' = [ 0 | _ <- [0..fromInteger (toInteger $ size-1)]]
          floatVector :: VU.Vector Float
          !floatVector = VU.fromList floatList
          floatVector' :: VU.Vector Float
          !floatVector' = VU.fromList floatList

          n :: Int
          n = toInt . sqrt $ fromIntegral size
          d = 0.5

toInt :: RealFrac a => a -> Int
toInt = fromIntegral . floor

stencilBench8 :: Int -> Int -> IO ()
stencilBench8 steps size = 
                    do !floatMutableVector  <- VU.thaw floatVector
                       !floatMutableVector' <- VU.thaw floatVector'
                       forM_ [0..steps `div` 2] $ \_ -> do
                         forM_ [1..n-2] $ \i -> do
                           forM_ [1..((n - 1) `div` 8)-1] $ \j -> do
                             north <- floatMutableVector `VGM.unsafeRead` (n*(i-1) + 8*j+0 )
                             east  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+0 - 1 )
                             here  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+0     )
                             west  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+0 + 1 )
                             south <- floatMutableVector `VGM.unsafeRead` (n*(i+1) + 8*j+0 )
                             VGM.unsafeWrite floatMutableVector' (n*i+8*j+0) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector `VGM.unsafeRead` (n*(i-1) + 8*j+1 )
                             east  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+1 - 1 )
                             here  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+1     )
                             west  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+1 + 1 )
                             south <- floatMutableVector `VGM.unsafeRead` (n*(i+1) + 8*j+1 )
                             VGM.unsafeWrite floatMutableVector' (n*i+8*j+1) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector `VGM.unsafeRead` (n*(i-1) + 8*j+2 )
                             east  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+2 - 1 )
                             here  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+2     )
                             west  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+2 + 1 )
                             south <- floatMutableVector `VGM.unsafeRead` (n*(i+1) + 8*j+2 )
                             VGM.unsafeWrite floatMutableVector' (n*i+8*j+2) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector `VGM.unsafeRead` (n*(i-1) + 8*j+3 )
                             east  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+3 - 1 )
                             here  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+3     )
                             west  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+3 + 1 )
                             south <- floatMutableVector `VGM.unsafeRead` (n*(i+1) + 8*j+3 )
                             VGM.unsafeWrite floatMutableVector' (n*i+8*j+3) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector `VGM.unsafeRead` (n*(i-1) + 8*j+4 )
                             east  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+4 - 1 )
                             here  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+4     )
                             west  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+4 + 1 )
                             south <- floatMutableVector `VGM.unsafeRead` (n*(i+1) + 8*j+4 )
                             VGM.unsafeWrite floatMutableVector' (n*i+8*j+4) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector `VGM.unsafeRead` (n*(i-1) + 8*j+5 )
                             east  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+5 - 1 )
                             here  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+5     )
                             west  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+5 + 1 )
                             south <- floatMutableVector `VGM.unsafeRead` (n*(i+1) + 8*j+5 )
                             VGM.unsafeWrite floatMutableVector' (n*i+8*j+5) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector `VGM.unsafeRead` (n*(i-1) + 8*j+6 )
                             east  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+6 - 1 )
                             here  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+6     )
                             west  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+6 + 1 )
                             south <- floatMutableVector `VGM.unsafeRead` (n*(i+1) + 8*j+6 )
                             VGM.unsafeWrite floatMutableVector' (n*i+8*j+6) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector `VGM.unsafeRead` (n*(i-1) + 8*j+7 )
                             east  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+7 - 1 )
                             here  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+7     )
                             west  <- floatMutableVector `VGM.unsafeRead` (n*i     + 8*j+7 + 1 )
                             south <- floatMutableVector `VGM.unsafeRead` (n*(i+1) + 8*j+7 )
                             VGM.unsafeWrite floatMutableVector' (n*i+8*j+7) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                         forM_ [1..n-2] $ \i -> do
                           forM_ [1..((n - 1) `div` 8)-1] $ \j -> do
                             north <- floatMutableVector' `VGM.unsafeRead` (n*(i-1) + 8*j+0 )
                             east  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+0 - 1 )
                             here  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+0     )
                             west  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+0 + 1 )
                             south <- floatMutableVector' `VGM.unsafeRead` (n*(i+1) + 8*j+0 )
                             VGM.unsafeWrite floatMutableVector (n*i+8*j+0) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector' `VGM.unsafeRead` (n*(i-1) + 8*j+1 )
                             east  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+1 - 1 )
                             here  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+1     )
                             west  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+1 + 1 )
                             south <- floatMutableVector' `VGM.unsafeRead` (n*(i+1) + 8*j+1 )
                             VGM.unsafeWrite floatMutableVector (n*i+8*j+1) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector' `VGM.unsafeRead` (n*(i-1) + 8*j+2 )
                             east  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+2 - 1 )
                             here  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+2     )
                             west  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+2 + 1 )
                             south <- floatMutableVector' `VGM.unsafeRead` (n*(i+1) + 8*j+2 )
                             VGM.unsafeWrite floatMutableVector (n*i+8*j+2) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector' `VGM.unsafeRead` (n*(i-1) + 8*j+3 )
                             east  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+3 - 1 )
                             here  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+3     )
                             west  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+3 + 1 )
                             south <- floatMutableVector' `VGM.unsafeRead` (n*(i+1) + 8*j+3 )
                             VGM.unsafeWrite floatMutableVector (n*i+8*j+3) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector' `VGM.unsafeRead` (n*(i-1) + 8*j+4 )
                             east  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+4 - 1 )
                             here  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+4     )
                             west  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+4 + 1 )
                             south <- floatMutableVector' `VGM.unsafeRead` (n*(i+1) + 8*j+4 )
                             VGM.unsafeWrite floatMutableVector (n*i+8*j+4) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector' `VGM.unsafeRead` (n*(i-1) + 8*j+5 )
                             east  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+5 - 1 )
                             here  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+5     )
                             west  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+5 + 1 )
                             south <- floatMutableVector' `VGM.unsafeRead` (n*(i+1) + 8*j+5 )
                             VGM.unsafeWrite floatMutableVector (n*i+8*j+5) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector' `VGM.unsafeRead` (n*(i-1) + 8*j+6 )
                             east  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+6 - 1 )
                             here  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+6     )
                             west  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+6 + 1 )
                             south <- floatMutableVector' `VGM.unsafeRead` (n*(i+1) + 8*j+6 )
                             VGM.unsafeWrite floatMutableVector (n*i+8*j+6) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
                             north <- floatMutableVector' `VGM.unsafeRead` (n*(i-1) + 8*j+7 )
                             east  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+7 - 1 )
                             here  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+7     )
                             west  <- floatMutableVector' `VGM.unsafeRead` (n*i     + 8*j+7 + 1 )
                             south <- floatMutableVector' `VGM.unsafeRead` (n*(i+1) + 8*j+7 )
                             VGM.unsafeWrite floatMutableVector (n*i+8*j+7) ((1-4*d) * here  
                                                                    + d*(  north
                                                                          + east
                                                                          + west
                                                                          + south
                                                                        ))
    where floatList :: [Float]
          !floatList = [0..fromInteger (toInteger $ size-1)]
          floatList' :: [Float]
          !floatList' = [ 0 | _ <- [0..fromInteger (toInteger $ size-1)]]
          floatVector :: VU.Vector Float
          !floatVector = VU.fromList floatList
          floatVector' :: VU.Vector Float
          !floatVector' = VU.fromList floatList

          n :: Int
          n = toInt . sqrt $ fromIntegral size
          d = 0.5

generatedStencilBench4 :: Int -> Int -> IO ()
generatedStencilBench4 steps size = do 
                              !floatMutableVector <- VU.thaw floatVector
                              !floatMutableVector' <- VU.thaw floatVector'
                              let rawa = VUSI.convertToRawVector floatMutableVector
                                  rawb = VUSI.convertToRawVector floatMutableVector'
                              primitive $ go rawa rawb
                                
    where     floatList :: [Float]
              !floatList = [0..fromInteger (toInteger $ size-1)]
              floatList' :: [Float]
              !floatList' = [ 0 | _ <- [0..fromInteger (toInteger $ size-1)]]
              floatVector :: VU.Vector Float
              !floatVector = VU.fromList floatList
              floatVector' :: VU.Vector Float
              !floatVector' = VU.fromList floatList
              n            = (toInt . sqrt $ fromIntegral size)
              !(I# n#)     = n
              !(I# n'#)    = n `div` 4
              !(I# steps#) = steps
              !(I# nborder#) = 4*n*(n `div` 4 - 1) 
              d            = broadcastFloatX4# 0.5#
              d#           = broadcastFloatX4# 0.5#
              go ::MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> State# RealWorld -> (# State# RealWorld, () #)
              go rawa rawb s = $(AFI.generate internalSolve) 
                      {- $ do 
                       let _d      = varE $ mkName "d"
                       for2D''' [| 0# |] [| steps# |] [| 1# |] 
                                [| 0# |] [| n'# |]    [| 1# |]  
                                [| 4# |] [| 4# *# n# |]     [| 4# |]
                                [| rawa |] [| rawb |] 
                            ( \_a -> \_b -> \_ -> \_i -> \_j -> do 
                             north  <-        _a `AFI.readFloatArrayAsFloatNorth` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) |]
                             east   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) -# 4#   |]
                             here   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j)         |]
                             west   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 4#   |]
                             south  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) +# 1#) +# $(_j) |]
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) |]  
                                                 [| (( (broadcastFloatX4# 1.0#) `minusFloatX4#` $(_d)) `timesFloatX4#` $(here))  
                                                    `plusFloatX4#` 
                                                    ($(_d)  `timesFloatX4#` (  $(north)
                                                              `plusFloatX4#`  $(east)
                                                              `plusFloatX4#`  $(west)
                                                              `plusFloatX4#`  $(south)
                                                                           )
                                                    ) |] 
                             return [| () |])
                            ( \_a -> \_b -> \_ -> \_i -> \_j -> do 
                             north  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) |]
                             east   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) -# 4#   |]
                             here   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j)         |]
                             west   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 4#   |]
                             south  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) +# 1#) +# $(_j) |]
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) |]  
                                                 [| (( (broadcastFloatX4# 1.0#) `minusFloatX4#` $(_d)) `timesFloatX4#` $(here))  
                                                    `plusFloatX4#` 
                                                    ($(_d)  `timesFloatX4#` (  $(north)
                                                              `plusFloatX4#`  $(east)
                                                              `plusFloatX4#`  $(west)
                                                              `plusFloatX4#`  $(south)
                                                                           )
                                                    ) |] 
                             return [| () |])
                            ( \_a -> \_b -> \_ -> \_i -> \_j -> do 
                             north  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) |]
                             east   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) -# 4#   |]
                             here   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j)         |]
                             west   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 4#   |]
                             south  <-        _a `AFI.readFloatArrayAsFloatNorth` [| 4# *# n# *# ($(_i) +# 1#) +# $(_j) |]
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) |]  
                                                 [| (( (broadcastFloatX4# 1.0#) `minusFloatX4#` $(_d)) `timesFloatX4#` $(here))  
                                                    `plusFloatX4#` 
                                                    ($(_d)  `timesFloatX4#` (  $(north)
                                                              `plusFloatX4#`  $(east)
                                                              `plusFloatX4#`  $(west)
                                                              `plusFloatX4#`  $(south)
                                                                           )
                                                    ) |] 
                             return [| () |])
                            )-}

generatedStencilBench8 :: Int -> Int -> IO ()
generatedStencilBench8 steps size = do 
                              !floatMutableVector <- VU.thaw floatVector
                              !floatMutableVector' <- VU.thaw floatVector'
                              let rawa = VUSI.convertToRawVector floatMutableVector
                                  rawb = VUSI.convertToRawVector floatMutableVector'
                              primitive $ go rawa rawb
                                
    where     floatList :: [Float]
              !floatList = [0..fromInteger (toInteger $ size-1)]
              floatList' :: [Float]
              !floatList' = [ 0 | _ <- [0..fromInteger (toInteger $ size-1)]]
              floatVector :: VU.Vector Float
              !floatVector = VU.fromList floatList
              floatVector' :: VU.Vector Float
              !floatVector' = VU.fromList floatList'
              n              = toInt . sqrt $ fromIntegral size
              !(I# n#)       = n
              !(I# n'#)      = n `div` 4 - 1
              !(I# nborder#) = 4*n*(n `div` 4 - 1) 
              !(I# steps#) = steps
              d            = broadcastFloatX4# 0.5#
              go ::MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> State# RealWorld -> (# State# RealWorld, () #)
              go rawa rawb s = $(AFI.generate $ do 
                       let _d      = varE $ mkName "d"
                       for2D''  [| 0# |] [| steps# |] [| 1# |] 
                                [| 1# |] [| n'# |]    [| 1# |]  
                                [| 4# |] [| 4# *# n# |]     [| 4# |]
                                [| rawa |] [| rawb |] 
                            ( \_a -> \_b -> \_ -> \_i -> \_j -> do 
                             north  <-        _a `AFI.readFloatArrayAsFloatNorth` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) |]
                             east   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) -# 4#   |]
                             here   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j)         |]
                             west   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 4#   |]
                             south  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) +# 1#) +# $(_j) |]
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) |]  
                                                 [| (( (broadcastFloatX4# 1.0#) `minusFloatX4#` $(_d)) `timesFloatX4#` $(here))  
                                                    `plusFloatX4#` 
                                                    ($(_d)  `timesFloatX4#` (  $(north)
                                                              `plusFloatX4#`  $(east)
                                                              `plusFloatX4#`  $(west)
                                                              `plusFloatX4#`  $(south)
                                                                           )
                                                    ) |] 
                             return [| () |])
                            ( \_a -> \_b -> \_ -> \_i -> \_j -> do 
                             north  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) |]
                             east   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) -# 4#   |]
                             here   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j)         |]
                             west   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 4#   |]
                             south  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) +# 1#) +# $(_j) |]
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) |]  
                                                 [| (( (broadcastFloatX4# 1.0#) `minusFloatX4#` $(_d)) `timesFloatX4#` $(here))  
                                                    `plusFloatX4#` 
                                                    ($(_d)  `timesFloatX4#` (  $(north)
                                                              `plusFloatX4#`  $(east)
                                                              `plusFloatX4#`  $(west)
                                                              `plusFloatX4#`  $(south)
                                                                           )
                                                    ) |] 
                             return [| () |])
                            ( \_a -> \_b -> \_ -> \_i -> \_j -> do 
                             north  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) |]
                             east   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) -# 4#   |]
                             here   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j)         |]
                             west   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 4#   |]
                             south  <-        _a `AFI.readFloatArrayAsFloatSouth` [| nborder# +# $(_j) |]
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) |]  
                                                 [| (( (broadcastFloatX4# 1.0#) `minusFloatX4#` $(_d)) `timesFloatX4#` $(here))  
                                                    `plusFloatX4#` 
                                                    ($(_d)  `timesFloatX4#` (  $(north)
                                                              `plusFloatX4#`  $(east)
                                                              `plusFloatX4#`  $(west)
                                                              `plusFloatX4#`  $(south)
                                                                           )
                                                    ) |] 
                             return [| () |])
                            ) 
                   
