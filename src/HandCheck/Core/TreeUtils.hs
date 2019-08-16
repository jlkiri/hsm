module HandCheck.Core.TreeUtils
  ( buildTree
  , buildTrees
  , countPureSeqs
  , isValid
  , isPair
  , isPairStrict
  , checkStrictTriple
  , checkSingle
  , checkPairsAndTriples
  , isPinfu
  , isAllPairs
  , validate
  , isTriple
  , Tree(..)
  ) where

import Data.List
import HandCheck.Core.Types
import HandCheck.Core.Groups
import HandCheck.Core.Hands

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Eq)

instance (Show a) => Show (Tree a) where
  show x = customShow x 0

customShow :: (Show a) => Tree a -> Int -> String
customShow Empty _ = ""
customShow (Node l v r) depth =
  replicate depth '\t' ++
  show v ++
  "\n" ++
  customShow l (depth + 1) ++ customShow r (depth + 1)

-- Tree building functions

-- span p xs is equivalent to (takeWhile p xs, dropWhile p xs)
buildTree :: [Tile] -> Tree Tile
buildTree [] = Empty
buildTree (x:xs) = Node (buildTree seq) x (buildTree equals)
  where (equals, seq) = span (==x) xs

buildTrees :: [Tile] -> [Tree Tile]
buildTrees tiles = buildTree <$> groupByKind (sort tiles)

-- Tree operations functions

countSeqs :: Tree Tile -> Int
countSeqs Empty = 0
countSeqs (Node l _ r) = 1 + countSeqs l

-- 1-3-5 -> 1, 1-2-3 -> 3, 1-1-2-3 -> 2, 1-2-2-3 -> 2, 1-2-3-4-5-6 -> 6
-- A valid tree will always have N(pureseq) `mod` 3 == 0
countPureSeqs :: Tree Tile -> Int
countPureSeqs Empty = 0
countPureSeqs tree = pureSeqs tree 0 root
    where
      root = getRootValue tree - 1
      pureSeqs Empty acc _ = acc
      pureSeqs (Node l v r) acc prev
        | isEmpty r && (curr - prev == 1) = pureSeqs l (acc + 1) curr
        | isPair r && (curr - prev == 1) = pureSeqs l (acc + 1) curr
        | otherwise = pureSeqs l acc curr
        where curr = getValue v

countNodes :: Tree Tile -> Int
countNodes Empty = 0
countNodes (Node l v r) = 1 + countNodes l + countNodes r

getRootValue :: Tree Tile -> Int
getRootValue Empty = 0
getRootValue (Node _ v _) =
  case v of
    (HT _) -> 0
    (ST val _) -> fromEnum val + 1

getValue :: Tile -> Int
getValue (ST val _) = fromEnum val + 1
getValue _ = 0

getChi :: Tree Tile -> [Tile]
getChi Empty = []
getChi node@(Node _ v _) 
  | isHonor v = []
  | sequential = getAllChis node
  | otherwise = []
  where sequential = countPureSeqs node `mod` 3 == 0

-- Works correctly only on valid trees
getAllChis :: Tree Tile -> [Tile]
getAllChis Empty = []
getAllChis node@(Node l v _)
  | isAllPairs node = v : v : getAllChis l
  | isPair node = getAllChis l
  | otherwise = v : getAllChis l

-- Tree validation functions

-- A hand is valid only if every tree is valid. Invalid trees have unfinished sets, which means
-- that given the fixed number of tiles in a hand, some other tree either has excess tiles or not enough
-- Ex: 1m2m + 3s4s5s6s

validate :: [Tile] -> [Tree Tile]
validate hand
  | valid = buildTrees hand
  | otherwise = []
  where valid = and $ isValid <$> buildTrees hand

isValid :: Tree Tile -> Bool
isValid Empty = False
isValid tree
  | checkSingle tree = False
  | otherwise = or $ sequenceA [
    checkForChis
  , checkPairsAndTriples
  , checkStrictTriple] tree

checkForChis :: Tree Tile -> Bool
checkForChis tree = hasChis && hasFinishedChis
  where chis = countPureSeqs tree
        hasChis = chis > 0
        hasFinishedChis = chis `mod` 3 == 0

checkSingle :: Tree Tile -> Bool
checkSingle = (==1) . countNodes

checkPairsAndTriples :: Tree Tile -> Bool
checkPairsAndTriples Empty = False
checkPairsAndTriples node@(Node l _ r)
  | isPairStrict node = True
  | isPair node || isTriple node = checkPairsAndTriples l
  | otherwise = False

checkStrictTriple :: Tree Tile -> Bool
checkStrictTriple (Node l _ r) =
  case r of
    Empty -> False
    _ -> isPair r && isEmpty l

-- False positive on 1-1-3 tree (do not use to detect strict pairs)
isPair :: Tree Tile -> Bool
isPair Empty = False
isPair (Node l _ r) =
  case r of
    Empty -> False
    _ -> not . isPair $ r

isAllPairs :: Tree Tile -> Bool
isAllPairs Empty = False
isAllPairs node@(Node l _ r)
  | isPairStrict node = True
  | isPair node = isAllPairs l
  | otherwise = False

isPinfu :: [Tree Tile] -> Bool
isPinfu trees = chiSum == 12
  where chiSum = sum $ length <$> getChi <$> trees

isPairStrict :: Tree Tile -> Bool
isPairStrict Empty = False
isPairStrict node@(Node l _ r) =
  case r of
    Empty -> False
    _ -> isPair node && isEmpty l

isEmpty :: Tree Tile -> Bool
isEmpty Empty = True
isEmpty _ = False

isTriple :: Tree Tile -> Bool
isTriple (Node l _ r) =
  case r of
    Empty -> False
    _ -> isPair r

isHonor :: Tile -> Bool
isHonor (HT _) = True
isHonor _ = False