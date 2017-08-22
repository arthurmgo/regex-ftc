module Sets where

import           Data.List hiding (union)

newtype Set a = SetI [a]

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f (SetI xs) = makeSet (map f xs)

makeSet :: Ord a => [a] -> Set a
makeSet = SetI . remDups . sort
  where
    remDups [] = []
    remDups [x] = [x]
    remDups (x:y:xs)
      | x < y = x : remDups (y:xs)
      | otherwise = remDups (y:xs)

card :: Set a -> Int
card (SetI xs)     = length xs

eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (SetI xs) (SetI ys) = xs == ys

showSet :: Show a => Set a -> String
showSet (SetI xs) = "{" ++ f xs ++ "}"
  where
    f []     = ""
    f [y]    = show y
    f (y:ys) = show y ++ ", " ++ f ys

flatten :: Set a -> [a]
flatten (SetI xs) = xs

empty :: Set aempty
empty  = SetI []

isEmpty :: (Eq a) => Set a -> Bool
isEmpty = (== empty)

sing :: a -> Set a
sing x = SetI [x]

union    :: Ord a => Set a -> Set a -> Set a
union (SetI xs) (SetI ys) = SetI (uni xs ys)

uni :: Ord a => [a] -> [a] -> [a]
uni [] ys = ys
uni xs [] = xs
uni (x:xs) (y:ys)
          | x<y       = x : uni xs (y:ys)
          | x==y      = x : uni xs ys
          | otherwise = y : uni (x:xs) ys

inter    :: Ord a => Set a -> Set a -> Set a
inter (SetI xs) (SetI ys) = SetI (int xs ys)

int    :: Ord a => [a] -> [a] -> [a]
int [] ys = []
int xs [] = []
int (x:xs) (y:ys)
         | x<y        = int xs (y:ys)
         | x==y       = x : int xs ys
         | otherwise  = int (x:xs) ys

setlimit :: Eq a => (Set a -> Set a) -> Set a -> Set a
setlimit f s
  | s == next = s
  | otherwise = setlimit f next
    where
      next = f s

instance Eq a => Eq (Set a) where
  (==) = eqSet

instance Ord a => Ord (Set a) where
  s1 <= s2 = flatten s1 <= flatten s2

instance Show a => Show (Set a) where
  show = showSet
