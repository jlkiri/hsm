module Types where

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