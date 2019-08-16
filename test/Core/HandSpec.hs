module Core.HandSpec
  ( spec 
  ) where

import Test.Hspec
import HandCheck.Core.TreeUtils
import HandCheck.Core.Hand
import HandCheck.Core.Hands

spec :: Spec
spec = describe "Hands" $ do
        it "Detects pinfu" $
          (isPinfu $ validate pinfuHand) `shouldBe` True

        it "Detects ambiguous pinfu" $
          (isPinfu $ validate ambiguousPinfuHand) `shouldBe` True

        it "Detects iipeiko" $
          (isIipeiko $ validate ambiguousPinfuHand) `shouldBe` True