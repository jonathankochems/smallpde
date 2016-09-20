{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module DataVectorUnboxedSimdSpec (spec) where

import Test.Hspec
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import Control.Monad

import Data.Vector.Unboxed.SIMD as VUS
import Data.Primitive.SIMD (unpackVector,packVector, FloatX4, unsafeInsertVector)

spec :: Spec
spec = do let floatList :: [Float]
              floatList = [0..127]
              floatVector :: VU.Vector Float
              floatVector = VU.fromList floatList
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


