import Test.QuickCheck
import HandCheck.Core.Types

genHonor :: Gen Honor
genHonor = oneof [return West
            , return South
            , return East
            , return North
            , return Red
            , return Green
            , return White]

instance Arbitrary Honor where
  arbitrary = genHonor

genValue :: Gen Value
genValue = oneof [return One
            , return Two
            , return Three
            , return Four
            , return Five
            , return Six
            , return Seven
            , return Eight
            , return Nine]

instance Arbitrary Value where
  arbitrary = genValue

genKind :: Gen Kind
genKind = oneof [return Pin
            , return Sou
            , return Man]

instance Arbitrary Kind where
  arbitrary = genKind

genTile :: Gen Tile
genTile = do
  kind <- arbitrary :: Gen Kind
  value <- arbitrary :: Gen Value
  honor <- arbitrary :: Gen Honor
  oneof [
      return $ ST value kind
    , return $ HT honor]

instance Arbitrary Tile where
  arbitrary = genTile