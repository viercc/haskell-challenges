module LibCounter
    ( largestPowersInt
    ) where

import           Data.List
import           Data.Semigroup

class Eq a => Iterable a where
    zer :: a
    inc :: a -> a
    dec :: a -> a

instance Iterable Int where
    zer = 0
    inc = succ
    dec = pred

largestPowersInt :: Int -> [Int]
largestPowersInt = largestPowers

largestPowers :: Iterable a => Int -> [a]
largestPowers n = unfoldr (Just . increment n) []

increment :: Iterable c => Int -> [Int] -> (c, [Int])
increment base = go
  where
    one = inc zer
    go [] = (one, [1])
    go (x:xs)
      | x == base - 1 = let ~(c, xs') = go xs
                        in (inc c, 0:xs')
      | otherwise  = (one, (x + 1):xs)

