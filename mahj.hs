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

data Set = Pair (Tile, Tile) | Seq (Tile, Tile, Tile) | Triple (Tile, Tile, Tile) deriving (Show)

findTriples :: [Tile] -> [Set]
findTriples (x:y:z:rest) =
  case (x,y,z) of
    (ST _ _, ST _ _, ST _ _) -> handleTriple x y z
    (HT hon1, HT hon2, HT hon3) -> if hon1 == hon2 && hon2 == hon3
                                  then Triple (x, y, z) : findTriples (y:z:rest)
                                  else findTriples (y:z:rest)
    _ -> findTriples (y:z:rest)
  where
    handleTriple (ST val1 k1)  (ST val2 k2)  (ST val3 k3)
      | k1 /= k2 || k2 /= k3 = findTriples (y:z:rest)
      | val1 < val2 && val2 < val3 = Seq (x, y, z) : findTriples (y:z:rest)
      | val1 == val2 && val2 == val3 = Triple (x, y, z) : findTriples (y:z:rest)
      | otherwise = findTriples (y:z:rest)
findTriples [x,y] = []
findTriples [x] = []

findPairs :: [Tile] -> [Set]
findPairs (x:y:rest) = 
  case (x,y) of
    (ST _ _, ST _ _) -> handlePair x y
    (HT hon1, HT hon2) -> if hon1 == hon2
                      then Pair (x, y) : findPairs rest
                      else findPairs rest
    _ -> findPairs rest
  where
    handlePair (ST val1 k1) (ST val2 k2)
      | k1 /= k2 = findPairs (y:rest)
      | val1 == val2 = Pair (x, y) : findPairs rest
      | otherwise = findPairs rest
findPairs [x] = []
findPairs [] = []

findSets :: [Tile] -> [Set]
findSets x = pairs ++ triples
  where pairs = findPairs x
        triples = findTriples x



sets1 = findSets $ sort unsorted1
sets2 = findSets $ sort unsorted2




















type Pair = (Tile, Tile)
type Triple = (Tile, Tile, Tile)

showVal :: Tile -> String
showVal (ST x _) = show x

isPair :: Pair -> Bool
isPair (tile1, tile2) = tile1 == tile2

isTriple :: Triple -> Bool
isTriple (tile1, tile2, tile3) = tile1 == tile2 && tile2 == tile3

isConsec :: Triple -> Bool
isConsec (ST v1  _, ST v2 _, ST v3 _) =
  v1 `compare` v2 == LT && v2 `compare` v3 == LT

-- spUnsorted = [HT South, HT West, HT West]
-- spSorted = sortBy (\(HT x ) (HT y ) -> x `compare` y) spUnsorted

-- test = isConsec (OrdTriple (ST One Man, ST Two Man, ST Three Man))
