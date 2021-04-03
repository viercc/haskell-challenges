Starting from the naive implementation, derive
the efficient algorithm!

```haskell
largestPower :: Int -> Int -> Int
largestPower n = go 0 where
    go !d m
        | r == 0    = go (d + 1) q
        | otherwise = d
        where (q, r) = m `quotRem` n

largestPowers :: Int -> Int -> [Int]
largestPowers n a = map (largestPower n) [a, 2 * a..]
```

The task is to calculate `largestPowers n n` efficiently.

```haskell
largestPowers n n = map (largestPower n) [n, 2 * n..]
```

The first observation is each element of `[n, 2 * n ..]`
is divisible by `n`. So the following holds:

```haskell
largestPowers n n
 = map (largestPower n) [n, 2 * n..]
 = map (1 +) $ map (largestPower n) [1, 2..]
```

Then,

```haskell
largestPowers n n
 = map (1 +) $
     [ largestPower n 1, largestPower n 2, largestPower n 3, ..., largestPower n (n-1), largestPower n n
       , 
