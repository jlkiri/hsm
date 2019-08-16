module Core.TreeSpec
  ( spec 
  ) where

import Test.Hspec
import Data.List
import HandCheck.Core.Groups
import HandCheck.Core.TreeUtils
import HandCheck.Core.Types
import HandCheck.Core.Hands

testTree =
  Node
    (Node 
      (Node Empty (ST Three Man) Empty)
      (ST Two Man)
      (Node
        Empty
        (ST Two Man)
        (Node
          Empty
          (ST Two Man)
          Empty
        )
      )
    )
    (ST One Man)
    (Node Empty (ST One Man) Empty)

--11
testStrictPair = Node Empty (ST One Man) (Node Empty (ST One Man) Empty)

--111
testStrictTriple = Node Empty (ST One Man) (Node Empty (ST One Man) (Node Empty (ST One Man) Empty))

-- 112233
testAllPairs = buildTree [ST One Man, ST One Man, ST Two Man, ST Two Man, ST Three Man, ST Three Man]

-- 1122233
testNotAllPairs = buildTree [ST One Man, ST One Man, ST Two Man, ST Two Man, ST Two Man, ST Three Man, ST Three Man]

--12223
testForSeqs = buildTree [ST One Man, ST Two Man, ST Two Man, ST Two Man, ST Three Man]

--1
testSingle = buildTree [HT West]

spec :: Spec
spec = describe "Trees" $ do
        it "Correctly builds a tree" $
          buildTree [ST One Man, ST One Man, ST Two Man, ST Two Man, ST Two Man, ST Three Man] `shouldBe` testTree

        it "Correctly detects a single tile tree" $
          checkSingle testSingle `shouldBe` True

        it "Correctly detects a strict pair" $
          isPairStrict testStrictPair `shouldBe` True

        it "Correctly detects a strict triple" $
          checkStrictTriple testStrictTriple `shouldBe` True

        it "Correctly counts sequences" $
          (countPureSeqs testForSeqs) `shouldBe` 3

        it "Correctly builds trees for whole hand" $
          (length $ buildTrees completeHand) `shouldBe` 4

        it "Detects 7 pairs as winning" $
          (and $ isValid <$> buildTrees sevenPairHand) `shouldBe` True

        it "Detects all triple hand as winning" $
          (and $ isValid <$> buildTrees allTripleHand) `shouldBe` True

        it "Detects tenpai as non-winning" $
          (and $ isValid <$> buildTrees tenpaiHand) `shouldBe` False
        
        it "Detects complicated tenpai as non-winning" $
          (and $ isValid <$> buildTrees hand13) `shouldBe` False

        it "Detects all triple hand as winning" $
          (and $ isValid <$> buildTrees allTripleHand) `shouldBe` True

        it "Detects all pairs" $
          isAllPairs testAllPairs `shouldBe` True
        
        it "Detects not all-pair hand as not all pairs" $
          isAllPairs testNotAllPairs `shouldBe` False
        
        it "Detects pairs and triples hand as pairs and triples" $
          checkPairsAndTriples testNotAllPairs `shouldBe` True