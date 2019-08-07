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

unsorted1 = [
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

unsorted2 = [
    ST Two Man
  , HT East
  , HT White
  , ST Two Man
  , ST Four Sou
  , ST Three Pin
  , ST One Man
  , ST Two Sou
  , ST Two Man
  , ST Three Man
  , HT South
  , ST Four Man
  , HT White
  , ST Three Sou]

niceHand = [
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

shortHand = [
  ST Four Man,
  ST Five Man,
  ST Six Man,
  ST Six Man]

data Set = Pair (Tile, Tile) | Seq (Tile, Tile, Tile) | Triple (Tile, Tile, Tile) deriving (Eq, Show)

preSetify :: [Tile] -> [[Tile]]
preSetify [] = [[]]
preSetify tiles = pairs `mappend` triples
  where pairs = subsets 2 tiles
        triples = subsets 3 tiles

makeTriples :: [Tile] -> Maybe Set
makeTriples [x,y,z] =
  case (x,y,z) of
    (ST _ _, ST _ _, ST _ _) -> handleTriple x y z
    (HT hon1, HT hon2, HT hon3) -> if hon1 == hon2 && hon2 == hon3
                                  then Just (Triple (x, y, z))
                                  else Nothing
    _ -> Nothing
  where
    handleTriple (ST val1 k1)  (ST val2 k2)  (ST val3 k3)
      | k1 /= k2 || k2 /= k3 = Nothing
      | (fromEnum val3 - fromEnum val2) == 1 && (fromEnum val2 - fromEnum val1) == 1 = Just (Seq (x, y, z))
      | val1 == val2 && val2 == val3 = Just (Triple (x, y, z))
      | otherwise = Nothing

makePairs :: [Tile] -> Maybe Set
makePairs [x, y] =
  case (x,y) of
    (ST _ _, ST _ _) -> handlePair x y
    (HT hon1, HT hon2) -> if hon1 == hon2
                        then Just (Pair (x, y))
                        else Nothing
    _ -> Nothing
  where
    handlePair (ST val1 k1) (ST val2 k2)
      | k1 /= k2 = Nothing
      | val1 == val2 = Just (Pair (x, y))
      | otherwise = Nothing

setify :: [[Tile]] -> [Maybe Set]
setify xs = pairSets `mappend` tripleSets
  where pairSets = makePairs <$> filter ((==2) . length) xs
        tripleSets = makeTriples <$> filter ((==3) . length) xs

subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

isWinningHand :: [Tile] -> Bool
isWinningHand = undefined

isSeq :: Set -> Bool
isSeq (Seq (_, _, _)) = True
isSeq _ = False

isTriple :: Set -> Bool
isTriple (Triple (_, _, _)) = True
isTriple _ = False

isPair :: Set -> Bool
isPair (Pair (_, _)) = True
isPair _ = False

validCombs :: [[Set]] -> [[Set]]
validCombs = filter valid
  where seqsOrTriples = length . filter (\x -> isSeq x || isTriple x)
        pairs = length . filter isPair
        valid x = pairs x == 1 && seqsOrTriples x == 3
