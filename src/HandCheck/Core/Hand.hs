module HandCheck.Core.Hand
  ( isPinfu
  , isIipeiko
  ) where

import Data.List
import HandCheck.Core.Types
import HandCheck.Core.TreeUtils
import HandCheck.Core.Hands

data Info = Info { chi :: [[Tile]]
                , kinds :: [Either Honor Kind]
                , pon :: [[Tile]] }

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

getKinds :: Tree Tile -> Either Honor Kind
getKinds (Node _ v _) = getKind v
  where getKind (ST _ k) = Right k
        getKind (HT k) = Left k

getPons :: Tree Tile -> [Tile]
getPons Empty = []
getPons node@(Node l v _)
  | isTriple node = v : v : v : getPons l
  | otherwise = getPons l

getInfo :: [Tree Tile] -> Info
getInfo trees = info
  where 
    chi = getChi <$> trees
    pon = getPons <$> trees
    kinds = getKinds <$> trees
    info = Info { chi = chi, pon = pon, kinds = kinds }

checkYaku :: [Tree Tile] -> String
checkYaku trees = checkYaku' info
  where info = getInfo trees

checkYaku' :: Info -> String
checkYaku' = do
  pinfu <- isPinfu
  iipeiko <- isIipeiko
  return $ "Pinfu: " <> pinfu <> " Iipeiko: " <> iipeiko

isIipeiko :: Info -> String
isIipeiko = (chi . id) >>= \chis -> return $ show $ or $ hasDoubleChi <$> chis
  where 
    nubbed = length . nub
    hasDoubleChi [] = False
    hasDoubleChi x = length x `div` nubbed x == 2

-- >=12 because of 11223344 cases where one tail is ambiguously a part of a pair
isPinfu :: Info -> String
isPinfu = (chi . id) >>= \chis -> return $ show $ chiSum chis >= 12
  where chiSum x = sum $ length <$> x