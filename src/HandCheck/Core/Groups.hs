module HandCheck.Core.Groups
  (
    groupByKind
  , sameKindAs
  ) where

import Data.List
import HandCheck.Core.Hands
import HandCheck.Core.Types

sameKindAs :: Tile -> Tile -> Bool
sameKindAs (ST _ k1) (ST _ k2) = k1 == k2
sameKindAs (HT h1) (HT h2) = h1 == h2
sameKindAs _ _ = False

isSuccT :: Tile -> Tile -> Bool
isSuccT (ST v1 _) (ST v2 _) = fromEnum v2 + 1 == fromEnum v1
isSuccT (HT _) (HT _) = True
isSuccT _ _ = False
 
f :: (Ord a, Num a, Eq a) => [a] -> [[a]]
f = foldr g []
  where g a [] = [[a]]
        g a xs@(x:xs')
          | a+1 == head x = (a : x) : xs'
          | otherwise = [a]:xs

groupByKind :: [Tile] -> [[Tile]]
groupByKind = foldr fn []
  where fn a [] = [[a]]
        fn a xs@(x:xs')
          | (h `isSuccT` a || h == a) && (a `sameKindAs` h) = (a : x) : xs'
          | otherwise = [a]:xs
          where h = head x
  