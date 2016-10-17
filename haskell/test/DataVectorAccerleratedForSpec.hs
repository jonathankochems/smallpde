{-# LANGUAGE ScopedTypeVariables, BangPatterns, MagicHash, UnboxedTuples, TemplateHaskell #-}
module DataVectorAccerleratedForSpec(spec) where

import Test.Hspec



import qualified Data.Vector.AcceleratedFor.Internal as AFI
import Data.Vector.AcceleratedFor.Internal (forAQ)
import Data.Vector.AcceleratedFor.Equivalence
import qualified Data.Vector.Unboxed.SIMD.Internal as VUSI

import qualified Data.Primitive.ByteArray

import Control.Monad.Primitive

import GHC.Prim
import qualified GHC.Tuple

import Language.Haskell.TH

import qualified Data.Vector.Unboxed.SIMD as VUS
import qualified Data.Vector.Unboxed.SIMD.Internal as VUSI
import Data.Primitive.SIMD (unpackVector,packVector, FloatX4, unsafeInsertVector)

spec :: Spec
spec = do let g :: State# RealWorld -> (# State# RealWorld, () #)
              g s = $(AFI.generate AFI.returnAQ)
              f :: IO ()
              f   = primitive g
              f' :: IO ()
              f'  = primitive h
              h :: State# RealWorld -> (# State# RealWorld, () #)
              h s = $(AFI.generate $ do _ <- AFI.returnAQ
                                        AFI.returnAQ)
              h' a i s = $(AFI.generate $ do let _a = varE $ mkName "a"
                                                 _i = varE $ mkName "i"
                                                 _start = varE $ mkName "start"
                                                 _end   = varE $ mkName "end"
                                                 _inc   = varE $ mkName "inc"
                                                 _row   = varE $ mkName "row"
                                                 _d     = varE $ mkName "d"
                                                 _d'    = varE $ mkName "d'"
                                             forAQ _start _end _inc $ \_index -> do 
                                                 north <- return <$> AFI.readFloatArrayAsFloatQ _a [|$(_index) -# $(_row)|]
                                                 east  <- return <$> AFI.readFloatArrayAsFloatQ _a [|$(_index) -# 4#|]
                                                 here  <- return <$> AFI.readFloatArrayAsFloatQ _a _index
                                                 west  <- return <$> AFI.readFloatArrayAsFloatQ _a [|$(_index) +# 4#|]
                                                 south <- return <$> AFI.readFloatArrayAsFloatQ _a [|$(_index) +# $(_row)|]
                                                 AFI.writeFloatArrayAsFloatQ _a _index 
                                                    [| ($(_d') `timesFloatX4#` $(here))  
                                                       `plusFloatX4#` 
                                                       ($(_d) `timesFloatX4#` (  $(north)
                                                                 `plusFloatX4#`  $(east)
                                                                 `plusFloatX4#`  $(west)
                                                                 `plusFloatX4#`  $(south)
                                                                              )
                                                       ) |] 
                                                 return $ tupE [])
                where {-# INLINE start #-}
                      start = 4#
                      {-# INLINE end #-}
                      end   = 20#
                      {-# INLINE row #-}
                      row   = 20#
                      {-# INLINE inc #-}
                      inc   = 4#
                      __d   = packVector (0.5,0.5,0.5,0.5)
                      {-# INLINE d #-}
                      !(VUSI.FloatX4# d) = VUSI.coerceToInternalFloatX4 __d
                      {-# INLINE d' #-}
                      !(VUSI.FloatX4# d') = VUSI.coerceToInternalFloatX4 $! (1-4*__d)

          describe "Unlifted Floats" $ do
            it "compare as expected" $ do
                  x <- f
                  x `shouldBe` ()
          describe "Components" $ do
            it "readFloatArray generates the right code" $ do 
                let a = varE $ mkName "a"
                    s = varE $ mkName "s"
                    base = [| case readFloatArrayAsFloatX4# $(a) 10 $(s) of
                                (# state_0, read1 #) -> (# state_0, $(tupE []) #) |]
                    read = AFI.generate $ do let _a = varE $ mkName "a"
                                             AFI.readFloatArrayAsFloatQ _a [| 10 |]
                assertT $ read `syntacticEqualsQ` base 
            it "writeFloatArray generates the right code" $ do 
                let a = varE $ mkName "a"
                    s = varE $ mkName "s"
                    base  = [| case writeFloatArrayAsFloatX4# $(a) 10 10 $(s) of
                                state_0 -> (# state_0, $(tupE []) #) |]
                    write = AFI.generate $ do let _a = varE $ mkName "a"
                                              AFI.writeFloatArrayAsFloatQ _a [| 10 |] [| 10 |]
                assertT $ write `syntacticEqualsQ` base 
            it ">>= generates the right code" $ do 
                let a = varE $ mkName "a"
                    s = varE $ mkName "s"
                    base  =  [| case readFloatArrayAsFloatX4# $(a) 10 $(s) of
                                 (# state_0, read1 #) -> 
                                   case writeFloatArrayAsFloatX4# $(a) 10 read1 state_0 of
                                     state_1 -> (# state_1, $(tupE []) #) |]
                    bind = AFI.generate $ do let _a = varE $ mkName "a"
                                             x <- return <$> AFI.readFloatArrayAsFloatQ _a [| 10 |]
                                             AFI.writeFloatArrayAsFloatQ _a [| 10 |] x
                assertT $ bind `syntacticEqualsQ` base
            it "for generates the right code" $ do 
                let a = varE $ mkName "a"
                    s = varE $ mkName "s"
                    base  =  [| let go_0 :: Int# -> State# RealWorld -> (# State# RealWorld, () #)
                                    go_0 i1 state_1 = case i1 <# 10 of
                                                        0# -> (# state_1, () #)
                                                        1# -> case writeFloatArrayAsFloatX4# $(a) i1 10 state_1 of
                                                                state_2 -> go (i1 +# 1) state_2
                                in case go_0 0 $(s) of
                                    (# state_3, () #) -> (# state_3, $(tupE []) #) |]
                    for   = AFI.generate $ do let _a = varE $ mkName "a"
                                              forAQ [| 0 |] [| 10 |] [| 1 |] $ \_i -> do
                                                AFI.writeFloatArrayAsFloatQ _a _i [| 10 |]
                                                return $ (tupE [])

                assertT $ for `syntacticEqualsQ` base

assertT x = do x' <- x
               x' `shouldBe` True
