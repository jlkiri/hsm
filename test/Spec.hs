import Test.Hspec
import HandCheck.Check

main :: IO ()
main = hspec $ do
  describe "Main" $ do
    it "correctly detects a win" $ do
      isWinning `shouldBe` True
