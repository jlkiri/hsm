module HandCheck.Core.TreeUtils
  ( buildTree
  , buildTrees
  , countPureSeqs
  , isValid
  , isPair
  , isTriple
  , Tree(..)
  ) where

import HandCheck.Core.Types

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

-- TODO: change isSequential to just == x + samekind (maybe filter (/=x) + samekind) (e.g. allow 1-3-5 trees)
buildTree :: [Tile] -> Tree Tile
buildTree [] = Empty
buildTree (x:xs) = Node (buildTree seq) x (buildTree equals)
  where seq = dropWhile (not . isSequential x) xs
        equals = filter (==x) xs

-- A hand is valid only if every tree is valid. Invalid trees have unfinished sets, which means
-- that given the fixed number of tiles in a hand, some other tree either has excess tiles or not enough
-- Ex: 1m2m + 3s4s5s6s

isValid :: Tree Tile -> Bool
isValid Empty = False
isValid tree = isPair tree || isTriple tree || (countPureSeqs tree 0 root) `mod` 3 == 0
  where root = getRootValue tree - 1

countSeqs :: Tree Tile -> Int
countSeqs Empty = 0
countSeqs (Node l _ r) = 1 + countSeqs l

getRootValue :: Tree Tile -> Int
getRootValue Empty = 0
getRootValue (Node _ v _) =
  case v of
    (HT _) -> 0
    (ST val _) -> fromEnum val + 1


-- false positive on 1-1-3 tree
isPair :: Tree Tile -> Bool
isPair Empty = False
isPair (Node l _ r) =
  case r of
    Empty -> False
    _ -> not . isPair $ r

-- false positive on 1-1-3 tree
isPairStrict :: Tree Tile -> Bool
isPairStrict Empty = False
isPairStrict (Node l _ r) =
  case r of
    Empty -> False
    _ -> isEmpty l

hasPair :: Tree Tile -> Bool
hasPair Empty = False
hasPair node@(Node l _ _)
  | isPair node = True
  | otherwise = hasPair l

isEmpty :: Tree Tile -> Bool
isEmpty Empty = True
isEmpty _ = False

isTriple :: Tree Tile -> Bool
isTriple (Node l _ r) =
  case r of
    Empty -> False
    _ -> isPair r && isEmpty l

getValue :: Tile -> Int
getValue (ST val _) = fromEnum val + 1

getAllChis :: Tree Tile -> [Tile]
getAllChis Empty = []
getAllChis node@(Node l v _)
  | isTriple node || isPair node = getAllChis l
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