data Tree a = Empty
  | Node (Tree a) a (Tree a)
  deriving (Eq)

instance (Show a) => Show (Tree a) where
  show x = customShow x 0

customShow :: (Show a) => Tree a -> Int -> String
customShow Empty _ = ""
customShow (Node l v r) depth =
  replicate depth '\t' ++
  show v ++
  "\n" ++
  customShow l (depth + 1) ++ customShow r (depth + 1)

buildTree :: (Num a, Ord a, Eq a) => [a] -> Tree a
buildTree [] = Empty
buildTree (x:xs) = Node (buildTree seq) x (buildTree equals)
  where seq = dropWhile (\y -> y - x /= 1) xs
        equals = filter (==x) xs

isWinning :: (Num a, Ord a, Eq a) => [a] -> Bool
isWinning xs = all (>1) $ numOfNodes <$> buildTrees xs

seqDepth :: Tree a -> Int
seqDepth Empty = 0
seqDepth (Node l _ r) = 1 + seqDepth l

numOfNodes :: Tree a -> Int
numOfNodes Empty = 0
numOfNodes (Node l v r) = 1 + numOfNodes l + numOfNodes r

buildTrees :: (Num a, Ord a, Eq a) => [a] -> [Tree a]
buildTrees [] = []
buildTrees xs = tree : (buildTrees $ drop n xs)
  where tree = buildTree xs
        n = numOfNodes tree
