module Lib
    ( largestPowersInt ) where

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
largestPowers n = aux (inc zer)
  where aux k = spread (n-1) k (aux (inc k))

-- > spread 2 '.' "apple" = "..a..p..p..l..e.."
spread :: Int -> a -> [a] -> [a]
spread n space as = spaces ++ (as >>= \a -> a : spaces)
  where spaces = replicate n space

