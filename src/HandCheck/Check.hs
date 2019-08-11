module HandCheck.Check
    ( isWinning
    ) where

import HandCheck.Core.Hands
import HandCheck.Core.TreeUtils
import HandCheck.Core.Types

isWinning :: Bool
isWinning = foldl (&&) True (isValid <$> buildTrees completeHand)
