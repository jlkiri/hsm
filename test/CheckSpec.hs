module CheckSpec
  ( spec 
  ) where

import Test.Hspec
import HandCheck.Check

spec :: Spec
spec = describe "Main" $ do
        it "Correctly detects a winning hand" $ do
          isWinning `shouldBe` True

        it "Correctly detects a non-winning hand" $ do
          isNotWinning `shouldBe` False