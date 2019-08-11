module Core.TreeSpec
  ( spec 
  ) where

import Test.Hspec
import Data.List
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

testPair = Node Empty (ST One Man) (Node Empty (ST One Man) Empty)

testTriple = Node Empty (ST One Man) (Node Empty (ST One Man) (Node Empty (ST One Man) Empty))

testForSeqs = buildTree [ST One Man, ST Two Man, ST Two Man, ST Two Man, ST Three Man]

spec :: Spec
spec = describe "Trees" $ do
        it "Correctly builds a tree" $
          buildTree [ST One Man, ST One Man, ST Two Man, ST Two Man, ST Two Man, ST Three Man] `shouldBe` testTree

        it "Correctly detects a pair" $
          isPair testPair `shouldBe` True

        it "Correctly detects a triple" $
          isTriple testTriple `shouldBe` True

        it "Correctly counts sequences" $
          (countPureSeqs testForSeqs 0 0) `shouldBe` 3

        it "Correctly builds trees for whole hand" $
          (length $ buildTrees completeHand) `shouldBe` 4

        it "Detects 7 pairs as winning" $
          all (==True) (isValid <$> sevenHand) `shouldBe` True

        it "Detects all triple hand as winning" $
          all (==True) (isValid <$> tripleHand) `shouldBe` True

        it "Detects tenpai as non-winning" $
          all (==True) (isValid <$> tenHand) `shouldBe` False
        
        it "Detects the hard one winning" $
          all (==True) (isValid <$> hardOne) `shouldBe` True

            where sevenHand = buildTrees $ sort sevenPairHand
                  tripleHand = buildTrees $ sort allTripleHand
                  tenHand = buildTrees $ sort tenpaiHand
                  hardOne = buildTrees $ sort myTestHand