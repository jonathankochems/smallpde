{-# LANGUAGE ScopedTypeVariables #-}
module SmallpdeSIMDSpec (main, spec) where

import Test.Hspec
-- import qualified Smallpde 
import SmallpdeVector (indexTransform)
import qualified SmallpdeSIMD as SmallpdeVector
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Primitive
import qualified Data.Primitive.ByteArray as ByteArray

-- import Data.Array.Repa (ix2, (!))

import Numeric (showEFloat)
import qualified DataVectorUnboxedSimdSpec as DVUSS

import Control.Monad

showFloat num = (showEFloat (Just 5) num "")          

main :: IO ()
main = do --hspec spec
          --hspec spec1
          hspec DVUSS.spec


spec1 :: Spec
spec1 = do describe "Smallpde" $
            it "solution should be in line with the baseline" $
               True `shouldBe` True

spec :: Spec
spec = do
    describe "Smallpde" $
      it "solution should be in line with the baseline" $
        do let smallBaseLine :: [Float]
               smallBaseLine = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                0, 2.40607e-08, 2.39306e-07, 1.58853e-06, 7.39248e-06, 2.46132e-05, 5.88457e-05, 0.000100254, 0.000120089, 0.000100254, 5.88457e-05, 2.46132e-05, 7.39248e-06, 1.58848e-06, 2.37843e-07, 0,
                                0, 2.39306e-07, 1.86265e-06, 1.03349e-05, 4.20832e-05, 0.000126877, 0.000282697, 0.000461193, 0.000544193, 0.000461193, 0.000282697, 0.000126877, 4.20832e-05, 1.03338e-05, 1.84155e-06, 0,
                                0, 1.58853e-06, 1.03349e-05, 4.98757e-05, 0.000182368, 0.000506575, 0.00106398, 0.00167351, 0.00194988, 0.00167351, 0.00106398, 0.000506575, 0.000182367, 4.98616e-05, 1.01546e-05, 0, 
                                0, 7.39248e-06, 4.20832e-05, 0.000182368, 0.000612967, 0.0015963, 0.00320025, 0.00489113, 0.00564292, 0.00489113, 0.00320025, 0.0015963, 0.00061296, 0.000182267, 4.10763e-05, 0, 
                                0, 2.46132e-05, 0.000126877, 0.000506575, 0.0015963, 0.00395629, 0.00765366, 0.0114442, 0.0131049, 0.0114442, 0.00765366, 0.00395629, 0.00159626, 0.000506116, 0.000123041, 0, 
                                0, 5.88457e-05, 0.000282697, 0.00106398, 0.00320025, 0.00765366, 0.0144322, 0.021245, 0.0241988, 0.021245, 0.0144322, 0.00765365, 0.00320011, 0.00106262, 0.000272598, 0, 
                                0, 0.000100254, 0.000461193, 0.00167351, 0.00489113, 0.0114442, 0.021245, 0.0309775, 0.0351709, 0.0309775, 0.021245, 0.0114442, 0.00489082, 0.00167084, 0.00044292, 0, 
                                0, 0.000120089, 0.000544193, 0.00194988, 0.00564292, 0.0131049, 0.0241988, 0.0351709, 0.0398886, 0.0351709, 0.0241988, 0.0131049, 0.00564251, 0.00194651, 0.000521829, 0, 
                                0, 0.000100254, 0.000461193, 0.00167351, 0.00489113, 0.0114442, 0.021245, 0.0309775, 0.0351709, 0.0309775, 0.021245, 0.0114442, 0.00489082, 0.00167084, 0.00044292, 0, 
                                0, 5.88457e-05, 0.000282697, 0.00106398, 0.00320025, 0.00765366, 0.0144322, 0.021245, 0.0241988, 0.021245, 0.0144322, 0.00765365, 0.00320011, 0.00106262, 0.000272598, 0, 
                                0, 2.46132e-05, 0.000126877, 0.000506575, 0.0015963, 0.00395629, 0.00765365, 0.0114442, 0.0131049, 0.0114442, 0.00765365, 0.00395628, 0.00159626, 0.000506116, 0.000123041, 0, 
                                0, 7.39248e-06, 4.20832e-05, 0.000182367, 0.00061296, 0.00159626, 0.00320011, 0.00489082, 0.00564251, 0.00489082, 0.00320011, 0.00159626, 0.000612953, 0.000182266, 4.10763e-05, 0, 
                                0, 1.58848e-06, 1.03338e-05, 4.98616e-05, 0.000182267, 0.000506116, 0.00106262, 0.00167084, 0.00194651, 0.00167084, 0.00106262, 0.000506116, 0.000182266, 4.98475e-05, 1.01535e-05, 0, 
                                0, 2.37843e-07, 1.84155e-06, 1.01546e-05, 4.10763e-05, 0.000123041, 0.000272598, 0.00044292, 0.000521829, 0.00044292, 0.000272598, 0.000123041, 4.10763e-05, 1.01535e-05, 1.82049e-06, 0, 
                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] 
           smallVector <- SmallpdeVector.solve 16 16
           forM_ [0..15] $ \i -> do  
              forM_ [0..15] $ \j -> do
                  -- putStr $ showFloat (smallVector VG.! indexTransform 16 i j) ++ " "
                  (x :: Float) <- smallVector `ByteArray.readByteArray` (indexTransform 16 i j)
                  x `shouldAlmostBe` (smallBaseLine !! (i*16+j))
{-    putStrLn ""
           putStrLn ""
           putStrLn ""
           forM_ [0..15] $ \i -> do  
              forM_ [0..15] $ \j -> do
                  putStr $ showFloat (smallBaseLine !! (i*16+j)) ++ " "
              putStrLn "" -}
    describe "one iteration" $
      it "should work fine" $
        do let smallBaseLine :: [Float]
               smallBaseLine = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.125, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.125, 0.5, 0.125, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.125, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
                                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0] 
           smallVector <- SmallpdeVector.solve 16 1 
           forM_ [0..3] $ \k -> do
             forM_ [0..3] $ \i -> do 
                forM_ [0..15] $ \j -> do
                    -- putStr $ show (smallVector VG.! (4*16*i+4*j+k)) ++ " "
                    (x :: Float) <- smallVector `ByteArray.readByteArray` (4*16*i+4*j+k)
                    x `shouldAlmostBe` (smallBaseLine !! ((4*k+i)*16+j))
                    --(smallVector VG.! (4*16*i+4*j+k)) `shouldAlmostBe` (smallBaseLine !! ((4*k+i)*16+j))
                -- putStrLn ""
    describe "the solution at step t" $ do 
      it "should be symmetric along the diagonal" $
        do smallVector <- SmallpdeVector.solve 20 8
           forM_ [0..3] $ \k -> do
             forM_ [0..4] $ \i -> do 
              forM_ [0..(5*k+i)] $ \j -> do 
                 let j'      = 5*k+i
                     (k',i') = j `divMod` 5
                 (x :: Float) <- smallVector `ByteArray.readByteArray` (4*20*i+4*j+k)
                 (y :: Float) <- smallVector `ByteArray.readByteArray` (4*20*i'+4*j'+k')
                 x `shouldAlmostBe` y
                 --(smallVector VG.! (4*20*i+4*j+k)) `shouldAlmostBe` (smallVector VG.! (4*20*i'+4*j'+k'))
      it "should be symmetric along the y-axis" $ 
        do smallVector <- SmallpdeVector.solve 20 7
           forM_ [0..1] $ \k -> do
             forM_ [0..4] $ \i -> do 
               forM_ [0..19] $ \j -> do
                 let (k',i')      = (5*(3-k)+(4-i)+1) `divMod` 5
                     j' = j
                 (x :: Float) <- smallVector `ByteArray.readByteArray` (4*20*i+4*j+k)
                 (y :: Float) <- smallVector `ByteArray.readByteArray` (4*20*i'+4*j'+k')
                 x `shouldAlmostBe` y
                 --(smallVector VG.! (4*20*i+4*j+k)) `shouldAlmostBe` (smallVector VG.! (4*20*i'+4*j'+k'))
      it "should be symmetric along the x-axis" $ 
        do smallVector <- SmallpdeVector.solve 20 7
           forM_ [0..3] $ \k -> do
             forM_ [0..4] $ \i -> do 
               forM_ [0..9] $ \j -> do
                 (x :: Float) <- smallVector `ByteArray.readByteArray` (4*20*i+4*(j+1)+k)
                 (y :: Float) <- smallVector `ByteArray.readByteArray` (4*20*i+4*(19-j)+k)
                 x `shouldAlmostBe` y 
                 --(smallVector VG.! (4*20*i+4*(j+1)+k)) `shouldAlmostBe` (smallVector VG.! (4*20*i+4*(19-j)+k))


shouldAlmostBe x y | diff < tolerance = True `shouldBe` True
                   | otherwise   = do putStrLn "Failure"
                                      putStrLn $ "x:         " ++ show x
                                      putStrLn $ "y:         " ++ show y
                                      putStrLn $ "diff:      " ++ show diff
                                      putStrLn $ "tolerance: " ++ show tolerance
                                      False `shouldBe` True
    where diff = abs (x - y)/(abs x `max` 1e-10)
          tolerance = 0.5*1e-5



