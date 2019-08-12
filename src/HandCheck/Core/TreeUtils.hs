module HandCheck.Core.TreeUtils
  ( buildTree
  , buildTrees
  , countPureSeqs
  , isValid
  , isPair
  , isTripleStrict
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

isSequential :: Tile -> Tile -> Bool
isSequential (ST val1 k1) (ST val2 k2) = k2 == k1 && (fromEnum val2 - fromEnum val1 == 1)
isSequential _ (HT _) = False

-- span p xs is equivalent to (takeWhile p xs, dropWhile p xs)
buildTree :: [Tile] -> Tree Tile
buildTree [] = Empty
buildTree (x:xs) = Node (buildTree seq) x (buildTree equals)
  where (equals, seq) = span (==x) xs

-- A hand is valid only if every tree is valid. Invalid trees have unfinished sets, which means
-- that given the fixed number of tiles in a hand, some other tree either has excess tiles or not enough
-- Ex: 1m2m + 3s4s5s6s

-- False negative on: [ST One Man, ST One Man, ST One Man, ST Two Man, ST Two Man, ST Two Man, ST Three Man, ST Three Man]
isValid :: Tree Tile -> Bool
isValid Empty = False
isValid tree
  | isSingleTile tree = False
  | isAllPairsOrTriples tree = True
  | isTripleStrict tree = True
  | hasSeqs && hasFinishedSeqs = True
  | otherwise = False
  where isSingleTile = (==1) . numOfNodes
        root = getRootValue tree - 1
        seqs = countPureSeqs tree 0 root
        hasSeqs = seqs /= 0
        hasFinishedSeqs = seqs `mod` 3 == 0

countSeqs :: Tree Tile -> Int
countSeqs Empty = 0
countSeqs (Node l _ r) = 1 + countSeqs l

getRootValue :: Tree Tile -> Int
getRootValue Empty = 0
getRootValue (Node _ v _) =
  case v of
    (HT _) -> 0
    (ST val _) -> fromEnum val + 1

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

isAllPairsOrTriples :: Tree Tile -> Bool
isAllPairsOrTriples Empty = False
isAllPairsOrTriples node@(Node l _ r)
  | isPairStrict node = True
  | isPair node || isTriple node = isAllPairsOrTriples l
  | otherwise = False

validate :: [Tile] -> [Tree Tile]
validate [] = []
validate hand
  | valid = builtHand
  | otherwise = []
  where builtHand = buildTree <$> groupByKind (sort hand)
        valid = all (==True) $ isValid <$> builtHand

isPinfu :: [Tree Tile] -> Bool
isPinfu [] = False
isPinfu trees = chiSum == 12
  where chiSum = sum $ length <$> getChi <$> trees

isPairStrict :: Tree Tile -> Bool
isPairStrict Empty = False
isPairStrict node@(Node l _ r) =
  case r of
    Empty -> False
    _ -> isPair node && isEmpty l

hasPair :: Tree Tile -> Bool
hasPair Empty = False
hasPair node@(Node l _ _)
  | isPair node = True
  | otherwise = hasPair l

isEmpty :: Tree Tile -> Bool
isEmpty Empty = True
isEmpty _ = False

isTripleStrict :: Tree Tile -> Bool
isTripleStrict (Node l _ r) =
  case r of
    Empty -> False
    _ -> isPair r && isEmpty l

isTriple :: Tree Tile -> Bool
isTriple (Node l _ r) =
  case r of
    Empty -> False
    _ -> isPair r

getValue :: Tile -> Int
getValue (ST val _) = fromEnum val + 1
getValue _ = 0

isHonor :: Tile -> Bool
isHonor (HT _) = True
isHonor _ = False

getChi :: Tree Tile -> [Tile]
getChi Empty = []
getChi node@(Node _ v _) 
  | isHonor v = []
  | sequential && symmetric = getAllChis node
  | otherwise = []
  where sequential = countSeqs node `mod` 3 == 0
        symmetric = numOfNodes node `mod` 3 == 0

-- Works correctly only on valid trees
getAllChis :: Tree Tile -> [Tile]
getAllChis Empty = []
getAllChis node@(Node l v _)
  | isAllPairs node = v : v : getAllChis l
  | isPair node = getAllChis l
  | otherwise = v : getAllChis l

-- 1-3-5 -> 1, 1-2-3 -> 3, 1-1-2-3 -> 2, 1-2-2-3 -> 2, 1-2-3-4-5-6 -> 6
-- A valid tree will always have N(pureseq) `mod` 3 == 0
countPureSeqs :: Tree Tile -> Int -> Int -> Int
countPureSeqs Empty acc _ = acc
countPureSeqs (Node l v r) acc prev
  | isEmpty r && (curr - prev == 1) = countPureSeqs l (acc + 1) curr
  | isPair r && (curr - prev == 1) = countPureSeqs l (acc + 1) curr
  | otherwise = countPureSeqs l acc curr
    where curr = getValue v

numOfNodes :: Tree Tile -> Int
numOfNodes Empty = 0
numOfNodes (Node l v r) = 1 + numOfNodes l + numOfNodes r

buildTrees :: [Tile] -> [Tree Tile]
buildTrees [] = []
buildTrees xs = tree : (buildTrees $ drop n xs)
  where tree = buildTree xs
        n = numOfNodes tree