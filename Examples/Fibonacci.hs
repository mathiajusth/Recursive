{-# LANGUAGE MultiParamTypeClasses #-}

module Examples.Fibonacci where

import Recursive
import Data.State

--------------------
-- Fibonacci
--------------------
newtype FibNat = FibNat Int

instance WithSubset FibNat where
  isInSubset (FibNat x) = x == 0 || x == 1

instance Recursive [] FibNat where
  recurse (FibNat x)
    | x >= 2    = [FibNat $ x - 2, FibNat $ x - 1]
    | otherwise = return . FibNat $ x

fibBase :: FibNat -> Int
fibBase (FibNat 0) = 0
fibBase (FibNat 1) = 1
fibBase _ = error "Not a base element"

fibStep :: FibNat -> [Int] -> Int
fibStep _ = sum

fib :: Int -> Int
fib n = assemble fibStep fibBase $ FibNat n

fibC :: Int -> State Count Int
fibC n = assembleWithCount fibStep fibBase $ FibNat n
