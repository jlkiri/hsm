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
isWinning = and $ isValid <$> buildTrees completeHand

isNotWinning :: Bool
isNotWinning = and $ isValid <$> buildTrees incompleteHand
