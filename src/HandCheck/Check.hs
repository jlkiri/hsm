module HandCheck.Check
    ( isWinning,
      isNotWinning
    ) where

import Data.List
import HandCheck.Core.Groups
import HandCheck.Core.Hands
import HandCheck.Core.TreeUtils
import HandCheck.Core.Types

isWinning :: Bool
isWinning = all (==True) (isValid <$> hand)
  where hand = buildTree <$> groupByKind (sort completeHand)

isNotWinning :: Bool
isNotWinning = all (==True) (isValid <$> hand)
  where hand = buildTree <$> groupByKind (sort incompleteHand)
