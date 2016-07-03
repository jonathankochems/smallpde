module SmallpdeSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "someFunction" $ 
    it "should work fine" $ 
      True `shouldBe` True
