{-# LANGUAGE ScopedTypeVariables #-}
module SmallpdeSpec (main, spec, spec1) where

import Test.Hspec
-- import qualified Smallpde 
import qualified SmallpdeVector
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU


-- import Data.Array.Repa (ix2, (!))

import Control.Monad

main :: IO ()
main = do hspec spec
          hspec spec1

spec1 :: Spec
spec1 = 
  describe "someFunction" $ 
    it "should work fine" $ 
      True `shouldBe` True

spec :: Spec
spec = do
    describe "" $
      it "should work fine" $
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
--           small <- Smallpde.solve 14 16
--           forM_ [1..14] $ \i -> 
--              forM_ [1..14] $ \j -> 
--                  small ! ix2 (i-1) (j-1) `shouldAlmostBe` (smallBaseLine !! (i*16+j))
           smallVector <- SmallpdeVector.solve 16 16 >>= VU.freeze
           forM_ [0..15] $ \i -> 
              forM_ [0..15] $ \j -> 
                  (smallVector VG.! (16*i + j)) `shouldAlmostBe` (smallBaseLine !! (i*16+j))

    describe "one iteration" $
      it "should work fine" $
        do let smallBaseLine :: [Float]
               smallBaseLine = [0, 0,     0,     0,     0, 
                                0, 0,     0.125, 0,     0,
                                0, 0.125, 0.5,   0.125, 0,
                                0, 0,     0.125, 0,     0,                                
                                0, 0,     0,     0,     0] 
--           small <- Smallpde.solve 3 1
--           forM_ [1..3] $ \i -> 
--              forM_ [1..3] $ \j -> 
--                  small ! ix2 (i-1) (j-1) `shouldBe` smallBaseLine !! (i*5+j)
           smallVector <- SmallpdeVector.solve 5 1 >>= VU.freeze
           forM_ [0..4] $ \i -> 
              forM_ [0..4] $ \j ->
                  --putStr $ show (smallVector VG.! (5*i + j)) ++ " "
                  (smallVector VG.! (5*i + j)) `shouldAlmostBe` (smallBaseLine !! (i*5+j))
              --putStrLn ""
    describe "the solution at step t" $ do 
      it "should be symmetric along the diagonal" $
        do small <- Smallpde.solve 17 16
--           forM_ [0..16] $ \i -> 
--              forM_ [0..i] $ \j -> 
--                  (small ! ix2 i j) `shouldAlmostBe` (small ! ix2 j i)
           smallVector <- SmallpdeVector.solve 19 16 >>= VU.freeze
           forM_ [0..18] $ \i -> 
              forM_ [0..i] $ \j -> 
                  (smallVector VG.! (19*i + j)) `shouldAlmostBe` (smallVector VG.! (19*j + i))
      it "should be symmetric along the y-axis" $ 
        do small <- Smallpde.solve 17 16
--           forM_ [0..8] $ \i -> 
--              forM_ [0..16] $ \j -> 
--                  (small ! ix2 i j) `shouldAlmostBe` (small ! ix2 (16-i) j)
           smallVector <- SmallpdeVector.solve 19 16 >>= VU.freeze
           forM_ [0..9] $ \i -> 
              forM_ [0..18] $ \j -> 
                  (smallVector VG.! (19*i + j)) `shouldAlmostBe` (smallVector VG.! (19*(18-i) + j))
      it "should be symmetric along the x-axis" $ 
        do -- small <- Smallpde.solve 17 16
           -- forM_ [0..16] $ \i -> 
           --   forM_ [0..8] $ \j -> 
           --       (small ! ix2 i j) `shouldAlmostBe` (small ! ix2 i (16-j))       
           smallVector <- SmallpdeVector.solve 19 16 >>= VU.freeze
           forM_ [0..18] $ \i -> 
              forM_ [0..9] $ \j -> 
                  (smallVector VG.! (19*i + j)) `shouldAlmostBe` (smallVector VG.! (19*i + 18-j))
 


shouldAlmostBe x y | diff < tolerance = True `shouldBe` True
                   | otherwise   = do putStrLn "Failure"
                                      putStrLn $ "x:         " ++ show x
                                      putStrLn $ "y:         " ++ show y
                                      putStrLn $ "diff:      " ++ show diff
                                      putStrLn $ "tolerance: " ++ show tolerance
                                      False `shouldBe` True
    where diff = abs (x - y)/(abs x `max` 1e-10)
          tolerance = 0.5*1e-5



