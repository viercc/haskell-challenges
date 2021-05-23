module Lib
    ( shortestLongest
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

-- | Matroska dolls of sisters.
--
-- The smallest (= 'Youngest') doll contains a value,
-- and a larger (= 'Elder') doll contains another doll. 
data Matroska a = Youngest a | Elder (Matroska a)

-- | Open up matroska dolls to get a value held inside. For example,
--
-- > openMatroska (Elder (Elder (Youngest x))) = x
openMatroska :: Matroska a -> a
openMatroska (Youngest x) = x
openMatroska (Elder xx) = openMatroska xx

-- | From a list @as@ with length /n/, create a set of /n/+1 matroska dolls,
--   holding a singleton list @[as]@ inside.
--
--   > listToMatroska [] = Youngest [[]]
--   > listToMatroska "ab" = Elder (Elder (Youngest ["ab"]))
--   > listToMatroska (repeat 'a') = fix Elder
--
--   For finite list @as@, @openMatroska (listToMatroska as) = [as]@.
listToMatroska :: [a] -> Matroska [[a]]
listToMatroska as = foldr (const Elder) (Youngest [as]) as

-- | Combine two sets of matroska dolls. If one of the sets have
--   different number of dolls, 'smaller' returns the one with
--   fewer dolls, discarding another.
--
--   If both sets have the same number of dolls, 'smaller' returns
--   a set with that number of dolls, holding a value combined with '<>'.
--
--   For example:
--
--   > smaller (Youngest x) (Elder (Elder (Youngest y))) = Youngest x
--   > smaller (Elder (Youngest x)) (Elder (Youngest y)) = Elder (Youngest (x <> y))
smaller :: Semigroup a => Matroska a -> Matroska a -> Matroska a
smaller (Youngest x) yy = case yy of
    Youngest y -> Youngest (x <> y)
    _          -> Youngest x
smaller (Elder _) (Youngest y) = Youngest y
smaller (Elder xx) (Elder yy) = Elder (smaller xx yy)

-- | Combine two sets of matroska dolls. If one of the sets have
--   different number of dolls, 'larger' returns the one with
--   more dolls, discarding another.
--
--   If both sets have the same number of dolls, 'larger' returns
--   a set with that number of dolls, holding a value combined with '<>'.
--
--   For example:
--
--   > larger (Youngest x) (Elder (Elder (Youngest y))) = Elder (Elder (Youngest y))
--   > larger (Elder (Youngest x)) (Elder (Youngest y)) = Elder (Youngest (x <> y))
larger :: Semigroup a => Matroska a -> Matroska a -> Matroska a
larger (Youngest x) (Youngest y) = Youngest (x <> y)
larger (Youngest _) yy = yy
larger (Elder xx') yy = Elder zz
  -- Be fully lazy! It's better to ensure:
  -- > larger (Elder xx') ⊥ = Elder ⊥
  where zz = case yy of
          Youngest _ -> xx'
          Elder yy'  -> larger xx' yy'

checkNonEmpty :: String -> [a] -> NonEmpty a
checkNonEmpty e as = case NE.nonEmpty as of
    Nothing -> error e
    Just as' -> as'

shortestLongest :: [[[a]]] -> [[a]]
shortestLongest = shortestLongest' . check . fmap check
  where check = checkNonEmpty "I don't want empty list!"

longest, shortest :: Semigroup a => NonEmpty (Matroska a) -> Matroska a
longest = foldr1 larger
shortest = foldr1 smaller

shortestLongest' :: NonEmpty (NonEmpty [a]) -> [[a]]
shortestLongest' = openMatroska . shortest . fmap (longest . fmap listToMatroska)
