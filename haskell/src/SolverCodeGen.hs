{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, TemplateHaskell, QuasiQuotes
           , ScopedTypeVariables, FlexibleContexts, GADTs, MagicHash,UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields -optc -ffast-math #-}

module SolverCodeGen where

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

import Data.Primitive.SIMD (unpackVector,packVector, FloatX4, unsafeInsertVector)

import Control.Monad

import Data.Primitive.SIMD (unpackVector,packVector, FloatX4, unsafeInsertVector)

import Language.Haskell.TH
import qualified Data.Vector.AcceleratedFor.Internal as AFI
import Data.Vector.AcceleratedFor.Internal (for1D,for2D,for2DSkip,for1D',for1D'',for2D'',for2D''',printQ,Accelerate(..))
import GHC.Prim
import Control.Monad.Primitive(primitive,primToIO,internal)
import GHC.Base (Int(..))

import qualified Data.Primitive.ByteArray as ByteArray
import VectorInst

when True  m = m
when False _ = return ()

internalSolve :: Accelerate ExpQ
internalSolve = do 
                       let _d      = varE $ mkName "d#"
                       -- printQ [| show ( "size"
                       --                , I# n#
                       --                , I# n'#
                       --                , I# nborder#
                       --                , I# nborder#
                       --                , VUSI.coerceFromFloatX4# $(_d)
                       --                ) |]
                       for2D''' [| 0# |] [| steps# |]   [| 1# |] 
                                [| 0# |] [| n'# |]      [| 1# |]  
                                [| 4# |] [| 4# *# n# |] [| 16# |]
                                [| rawa |] [| rawb |] 
                            ( \_a -> \_b -> \_ -> \_i -> \_j -> do 
                             north  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) +# 1#) +# $(_j) |]
                             east   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) -# 4#   |]
                             here   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j)         |]
                             west   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 4#   |]
                             south  <-        _a `AFI.readFloatArrayAsFloatSouth` [| nborder# +# $(_j) |]
                             -- printQ [| show ( "prologue"
                             --                , I# $(_i), I# $(_j), I# (4# *# n# *# $(_i) +# $(_j))
                             --                , VUSI.coerceFromFloatX4# $(north)
                             --                , VUSI.coerceFromFloatX4# $(east)
                             --                , VUSI.coerceFromFloatX4# $(here)
                             --                , VUSI.coerceFromFloatX4# $(west)
                             --                , VUSI.coerceFromFloatX4# $(south) 
                             --                , VUSI.coerceFromFloatX4# ((( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                             --                            ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                             --                        `timesFloatX4#` $(here))  
                             --                        `plusFloatX4#` 
                             --                        ($(_d)  `timesFloatX4#` (  $(north)
                             --                                  `plusFloatX4#`  $(east)
                             --                                  `plusFloatX4#`  $(west)
                             --                                  `plusFloatX4#`  $(south)
                             --                                               )
                             --                        )) 
                             --                ) |]
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) |]  
                                                 [| let new# = (( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                                                                    ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                                                                `timesFloatX4#` $(here))  
                                                                `plusFloatX4#` 
                                                                ($(_d)  `timesFloatX4#` (  $(north)
                                                                          `plusFloatX4#`  $(east)
                                                                          `plusFloatX4#`  $(west)
                                                                          `plusFloatX4#`  $(south)
                                                                                       ))
                                                    in insertFloatX4# new# 0.0# 0# 
                                                 |] 
                             return [| () |])
                            ( \_a -> \_b -> \_ -> \_i -> \_j -> do 
                             north0  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) +# 1#) +# $(_j) |]
                             north1  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) +# 1#) +# $(_j) +# 4#|]
                             north2  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) +# 1#) +# $(_j) +# 8#|]
                             north3  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) +# 1#) +# $(_j) +# 12#|]
                             east   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) -# 4#   |]
                             here   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j)         |]
                             west0   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 4#   |]
                             west1   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 8#   |]
                             west2   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 12#   |]
                             west3   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 16#   |]
                             south0  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) |]
                             south1  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) +# 4#|]
                             south2  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) +# 8#|]
                             south3  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) +# 12#|]
                             -- printQ [| show ( "body"
                             --                , I# $(_i), I# $(_j), I# (4# *# n# *# $(_i) +# $(_j))
                             --                , VUSI.coerceFromFloatX4# $(north)
                             --                , VUSI.coerceFromFloatX4# $(east)
                             --                , VUSI.coerceFromFloatX4# $(here)
                             --                , VUSI.coerceFromFloatX4# $(west)
                             --                , VUSI.coerceFromFloatX4# $(south) 
                             --                , VUSI.coerceFromFloatX4# ((( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                             --                            ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                             --                        `timesFloatX4#` $(here))  
                             --                        `plusFloatX4#` 
                             --                        ($(_d)  `timesFloatX4#` (  $(north)
                             --                                  `plusFloatX4#`  $(east)
                             --                                  `plusFloatX4#`  $(west)
                             --                                  `plusFloatX4#`  $(south)
                             --                                               )
                             --                        )) 
                             --                ) |]
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) |]  
                                                 [| (( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                                                        ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                                                    `timesFloatX4#` $(here))  
                                                    `plusFloatX4#` 
                                                    ($(_d)  `timesFloatX4#` (  $(north0)
                                                              `plusFloatX4#`  $(east)
                                                              `plusFloatX4#`  $(west0)
                                                              `plusFloatX4#`  $(south0)
                                                                           )
                                                    ) |] 
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) +# 4#|]  
                                                 [| (( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                                                        ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                                                    `timesFloatX4#` $(west0))  
                                                    `plusFloatX4#` 
                                                    ($(_d)  `timesFloatX4#` (  $(north1)
                                                              `plusFloatX4#`  $(here)
                                                              `plusFloatX4#`  $(west1)
                                                              `plusFloatX4#`  $(south1)
                                                                           )
                                                    ) |] 
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) +# 8# |]  
                                                 [| (( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                                                        ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                                                    `timesFloatX4#` $(west1))  
                                                    `plusFloatX4#` 
                                                    ($(_d)  `timesFloatX4#` (  $(north2)
                                                              `plusFloatX4#`  $(west0)
                                                              `plusFloatX4#`  $(west2)
                                                              `plusFloatX4#`  $(south2)
                                                                           )
                                                    ) |] 
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) +# 12# |]  
                                                 [| (( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                                                        ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                                                    `timesFloatX4#` $(west2))  
                                                    `plusFloatX4#` 
                                                    ($(_d)  `timesFloatX4#` (  $(north3)
                                                              `plusFloatX4#`  $(west1)
                                                              `plusFloatX4#`  $(west3)
                                                              `plusFloatX4#`  $(south3)
                                                                           )
                                                    ) |] 
                             return [| () |])
                            ( \_a -> \_b -> \_ -> \_i -> \_j -> do 
                             north  <-        _a `AFI.readFloatArrayAsFloatNorth` [| 0# +# $(_j) |]
                             east   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) -# 4#   |]
                             here   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j)         |]
                             west   <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# $(_i) +# $(_j) +# 4#   |]
                             south  <- return <$> _a `AFI.readFloatArrayAsFloatQ` [| 4# *# n# *# ($(_i) -# 1#) +# $(_j) |]
                             -- printQ [| show ( "epilogue"
                             --                , I# $(_i), I# $(_j), I# (4# *# n# *# $(_i) +# $(_j))
                             --                , VUSI.coerceFromFloatX4# $(north)
                             --                , VUSI.coerceFromFloatX4# $(east)
                             --                , VUSI.coerceFromFloatX4# $(here)
                             --                , VUSI.coerceFromFloatX4# $(west)
                             --                , VUSI.coerceFromFloatX4# $(south) 
                             --                , VUSI.coerceFromFloatX4# ((( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                             --                            ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                             --                        `timesFloatX4#` $(here))  
                             --                        `plusFloatX4#` 
                             --                        ($(_d)  `timesFloatX4#` (  $(north)
                             --                                  `plusFloatX4#`  $(east)
                             --                                  `plusFloatX4#`  $(west)
                             --                                  `plusFloatX4#`  $(south)
                             --                                               )
                             --                        )) 
                             --                ) |]
                             AFI.writeFloatArrayAsFloatQ _b [| 4# *# n# *# $(_i) +# $(_j) |]  
                                                 [| let new# = (( (broadcastFloatX4# 1.0#) `minusFloatX4#` 
                                                                    ( (broadcastFloatX4# 4.0#) `timesFloatX4#` $(_d)))
                                                                `timesFloatX4#` $(here))  
                                                                `plusFloatX4#` 
                                                                ($(_d)  `timesFloatX4#` (  $(north)
                                                                          `plusFloatX4#`  $(east)
                                                                          `plusFloatX4#`  $(west)
                                                                          `plusFloatX4#`  $(south)
                                                                                       ))
                                                     in insertFloatX4# new# 0.0# 3# 
                                                 |] 
                             return [| () |])
                            
