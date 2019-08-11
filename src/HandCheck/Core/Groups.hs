module HandCheck.Core.Groups where

import Data.List
import HandCheck.Core.Hands
import HandCheck.Core.Types

isSameKind :: Tile -> Tile -> Bool
isSameKind (ST _ k1) (ST _ k2) = k1 == k2
isSameKind (HT h1) (HT h2) = h1 == h2
isSameKind _ _ = False

-- memo: span p xs is equivalent to (takeWhile p xs, dropWhile p xs)
groupByKind :: [Tile] -> [[Tile]]
groupByKind [] = []
groupByKind xs@(x:_) = same : (groupByKind diff)
  where (same, diff) = span (\a -> x `isSameKind` a) xs


