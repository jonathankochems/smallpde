{-# LANGUAGE ScopedTypeVariables, BangPatterns, MagicHash, UnboxedTuples, TemplateHaskell #-}
module DataVectorAccerleratedForSpec(spec) where

import Test.Hspec



import qualified Data.Vector.AcceleratedFor.Internal as AFI
import Data.Vector.AcceleratedFor.Internal (forAQ)
import qualified Data.Vector.Unboxed.SIMD.Internal as VUSI

import Control.Monad.Primitive

import GHC.Prim 

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
                                                 north' <- AFI.readFloatArrayAsFloatQ _a [|$(_index) -# $(_row)|]
                                                 east'  <- AFI.readFloatArrayAsFloatQ _a [|$(_index) -# 4#|]
                                                 here'  <- AFI.readFloatArrayAsFloatQ _a _index
                                                 west'  <- AFI.readFloatArrayAsFloatQ _a [|$(_index) +# 4#|]
                                                 south' <- AFI.readFloatArrayAsFloatQ _a [|$(_index) +# $(_row)|]
                                                 let north = return north'
                                                     east  = return east' 
                                                     here  = return here' 
                                                     west  = return west' 
                                                     south = return south'
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
            
{-
      
          -}


