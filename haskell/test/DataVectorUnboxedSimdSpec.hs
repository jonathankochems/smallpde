{-# LANGUAGE ScopedTypeVariables, BangPatterns, MagicHash #-}
module DataVectorUnboxedSimdSpec (spec) where

import Test.Hspec
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import Control.Monad

import Data.Vector.Unboxed.SIMD as VUS
import qualified Data.Vector.Unboxed.SIMD.Internal as VUSI
import Data.Primitive.SIMD (unpackVector,packVector, FloatX4, unsafeInsertVector)

import GHC.Prim
import GHC.Int

spec :: Spec
spec = do let floatList :: [Float]
              floatList = [0..127]
              floatVector :: VU.Vector Float
              floatVector = VU.fromList floatList
          describe "Unlifted Floats" $ do
            it "compare as expected" $ do
               let zero#  = VUSI.unI# 0 
                   one#   = VUSI.unI# 1
                   two#   = VUSI.unI# 2
               True `shouldBe` (case zero# ># one# of
                                  1#  -> False
                                  0# -> True)
          describe "Vectors" $ do
            it "should have the same length as the lists used to initialised them" $
              do VU.length floatVector `shouldBe` length floatList
            it "should read the same elements as the lists used to initialised them" $
              do !floatMutableVector <- VU.thaw floatVector
                 forM_ [0..127] $ \i -> do
                    f <- floatMutableVector `VGM.read` i 
                    f `shouldBe` floatList !! i 
            it "when doing vectorised reads should read the same elements as the lists used to initialised them" $
              do !floatMutableVector <- VU.thaw floatVector
                 forM_ [0..31] $ \i -> do
                    f <- floatMutableVector `VUS.vectorisedRead` (i*4)
                    unpackVector f `shouldBe` ( floatList !! (4*i  )
                                              , floatList !! (4*i+1)
                                              , floatList !! (4*i+2)
                                              , floatList !! (4*i+3))
            it "when doing unsafe vectorised reads should read the same elements as the lists used to initialised them" $
              do !floatMutableVector <- VU.thaw floatVector
                 forM_ [0..31] $ \i -> do
                    f <- floatMutableVector `VUS.unsafeVectorisedRead` (i*4)
                    unpackVector f `shouldBe` ( floatList !! (4*i  )
                                              , floatList !! (4*i+1)
                                              , floatList !! (4*i+2)
                                              , floatList !! (4*i+3))
            it "when doing very unsafe vectorised reads should read the same elements as the lists used to initialised them" $
              do !floatMutableVector <- VU.thaw floatVector
                 forM_ [0..31] $ \i -> do
                    f <- floatMutableVector `VUS.veryunsafeVectorisedRead` (i*4)
                    unpackVector f `shouldBe` ( floatList !! (4*i  )
                                              , floatList !! (4*i+1)
                                              , floatList !! (4*i+2)
                                              , floatList !! (4*i+3))
            it "should correctly perform writes on elements" $
              do !floatMutableVector <- VU.thaw floatVector
                 forM_ [0..127] $ \i -> do
                    f <- floatMutableVector `VGM.read` i 
                    VGM.write floatMutableVector i (2*f) 
                    f' <- floatMutableVector `VGM.read` i 
                    f' `shouldBe` 2*floatList !! i 
            it "should correctly perform vectorised writes on elements" $
              do !floatMutableVector <- VU.thaw floatVector
                 forM_ [0..31] $ \i -> do
                    f <- floatMutableVector `VUS.vectorisedRead` (4*i) 
                    VUS.vectorisedWrite floatMutableVector (4*i) (2*f) 
                 forM_ [0..127] $ \i -> do
                    f' <- floatMutableVector `VGM.read` i 
                    f' `shouldBe` 2*floatList !! i 
            it "should correctly perform unsafe vectorised writes on elements" $
              do !floatMutableVector <- VU.thaw floatVector
                 forM_ [0..31] $ \i -> do
                    f <- floatMutableVector `VUS.vectorisedRead` (4*i) 
                    VUS.unsafeVectorisedWrite floatMutableVector (4*i) (2*f) 
                 forM_ [0..127] $ \i -> do
                    f' <- floatMutableVector `VGM.read` i 
                    f' `shouldBe` 2*floatList !! i 
            it "should correctly perform very unsafe vectorised writes on elements" $
              do !floatMutableVector <- VU.thaw floatVector
                 forM_ [0..31] $ \i -> do
                    f <- floatMutableVector `VUS.vectorisedRead` (4*i) 
                    VUS.veryunsafeVectorisedWrite floatMutableVector (4*i) (2*f) 
                 forM_ [0..127] $ \i -> do
                    f' <- floatMutableVector `VGM.read` i 
                    f' `shouldBe` 2*floatList !! i 
          describe "Stencil" $ do
            it "should do a weighted average of closeby points" $
              do let initialList   :: [Float]
                     initialList   = [0 | _ <- [0..35]]
                     initialVector :: VU.Vector Float
                     -- initialVector = VU.fromList initialList
                     !initialVector = VU.create (do !a <- VGM.new 36; VGM.set a 0; return a)
                     outputVector :: VU.Vector Float
                     -- outputVector  = VU.fromList initialList
                     !outputVector = VU.create (do !b <- VGM.new 36; VGM.set b 0; return b)
                 !initialMutableVector <- VU.thaw initialVector
                 !outputMutableVector  <- VU.thaw outputVector
                 let rawb = VUSI.convertToRawVector initialMutableVector
                     rawa = VUSI.convertToRawVector outputMutableVector
                     d :: FloatX4
                     d    = packVector (0.125,0.125,0.125,0.125)
                     !(VUSI.FloatX4# d#)  = VUSI.coerceToInternalFloatX4 d
                     !(VUSI.FloatX4# d'#) = VUSI.coerceToInternalFloatX4 $! (1-4*d)
                 VGM.write initialMutableVector 13 (1.0) 
                 VGM.write initialMutableVector 20 (1.0) 
                 forM_ [0..2] $ \i -> do
                   forM_ [0..11] $ \j -> do
                     f' <- initialMutableVector `VGM.read` (12*i+j)
                     putStr $ show f' ++ " " 
                   putStrLn ""
                 VUSI.rawVectorisedStencil 3 d# d'# rawa rawb 12 4
                 VUSI.rawVectorisedStencil 3 d# d'# rawa rawb 12 4
                 forM_ [0..2] $ \i -> do
                   forM_ [0..11] $ \j -> do
                     f' <- outputMutableVector `VGM.read` (12*i+j)
                     putStr $ show f' ++ " " 
                   putStrLn ""
