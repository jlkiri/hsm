import Data.List

data Honor = West | South | East | North | Red | Green | White deriving (Eq, Ord, Show)

data Value = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Enum, Eq, Ord)

data Kind = Man | Pin | Sou deriving (Eq, Ord)

data Tile = ST Value Kind | HT Honor deriving (Eq)

instance Show Kind where
  show Pin = "p"
  show Man = "m"
  show Sou = "s"

instance Show Value where
  show x = show $ fromEnum x + 1

instance Show Tile where
  show (ST val kind) = show val <> show kind
  show (HT tile) = show tile

instance Ord Tile where
  compare (ST val1 kind1) (ST val2 kind2)
    | (val1 > val2) && (kind1 == kind2) = GT
    | (val1 > val2) && (kind1 > kind2) = GT
    | (val1 < val2) && (kind1 > kind2) = GT
    | (val1 == val2) && (kind1 > kind2) = GT
    | (val1 == val2) && (kind1 == kind2) = EQ
    | otherwise = LT
  compare (HT hon1) (HT hon2) = compare hon1 hon2
  compare (HT _) (ST _ _) = GT
  compare (ST _ _) (HT _) = LT

isSameKind :: Tile -> Tile -> Bool
isSameKind (ST _ k1) (ST _ k2) = k1 == k2
isSameKind (HT h1) (HT h2) = h1 == h2
isSameKind _ _ = False

-- memo: span p xs is equivalent to (takeWhile p xs, dropWhile p xs)
groupByKind :: [Tile] -> [[Tile]]
groupByKind [] = []
groupByKind xs@(x:_) = same : (groupByKind diff)
  where (same, diff) = span (\a -> x `isSameKind` a) xs

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
isSequential (ST val1 _) (ST val2 _) = fromEnum val2 - fromEnum val1 == 1
isSequential _ (HT _) = False

buildTree :: [Tile] -> Tree Tile
buildTree [] = Empty
buildTree (x:xs) = Node (buildTree seq) x (buildTree equals)
  where seq = dropWhile (not . isSequential x) xs
        equals = filter (==x) xs

isWinning :: [Tile] -> Bool
isWinning xs = all (>1) $ numOfNodes <$> buildTrees xs

seqDepth :: Tree Tile -> Int
seqDepth Empty = 0
seqDepth (Node l _ r) = 1 + seqDepth l

numOfNodes :: Tree Tile -> Int
numOfNodes Empty = 0
numOfNodes (Node l v r) = 1 + numOfNodes l + numOfNodes r

buildTrees :: [Tile] -> [Tree Tile]
buildTrees [] = []
buildTrees xs = tree : (buildTrees $ drop n xs)
  where tree = buildTree xs
        n = numOfNodes tree

notWinningHand = [
    ST Two Man
  , HT East
  , HT White
  , ST Four Pin
  , ST Four Sou
  , ST Three Pin
  , ST One Man
  , ST Two Sou
  , ST Five Pin
  , ST Three Man
  , HT South
  , ST Four Man
  , HT White
  , ST Three Sou]

winningHand = [
  ST One Man,
  ST One Man,
  ST Two Man,
  ST Two Man,
  ST Three Man,
  ST Three Man,
  ST Four Man,
  ST Five Man,
  ST Six Man,
  ST Seven Man,
  ST Eight Man,
  HT White,
  HT White,
  ST Seven Man]

data Set = Pair (Tile, Tile) | Seq (Tile, Tile, Tile) | Triple (Tile, Tile, Tile) deriving (Eq, Show)


