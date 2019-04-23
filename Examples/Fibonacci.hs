{-# LANGUAGE MultiParamTypeClasses #-}

module Examples.Fibonacci where

import Recursive
import Data.State

--------------------
-- Fibonacci
--------------------
instance BaseOf Int where
  isBasal x = x == 0 || x == 1

instance Recursive [] Int where
  recurse x
    | x >= 2    = [x - 2, x - 1]
    | otherwise = return x

fibBase :: Int -> Int
fibBase 0 = 0
fibBase 1 = 1
fibBase _ = error "Not a base element"

fibStep :: Int -> [Int] -> Int
fibStep _ = sum

fib :: Int -> Int
fib = assemble fibStep fibBase

fibC :: Int -> State Count Int
fibC = assembleWithCount fibStep fibBase
