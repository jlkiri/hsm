import Data.List

data Set = Pair (Int, Int) | Triple (Int, Int, Int) deriving (Show)

findTriples :: [Int] -> [Set]
findTriples (x:y:z:rest)
  | (x < y && y < z) || (x == y && y == z) = Triple (x,y,z) : findTriples (y:z:rest)
  | otherwise = findTriples rest
findTriples [x,y] = []
findTriples [x] = []

findPairs :: [Int] -> [Set]
findPairs (x:y:rest)
  | x == y = Pair (x,y) : findPairs rest
  | otherwise = findPairs rest
findPairs [x] = []
findPairs [] = []

findSets :: [Int] -> [Set]
findSets x = pairs ++ triples
  where pairs = findPairs x
        triples = findTriples x