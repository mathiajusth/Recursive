{-# LANGUAGE MultiParamTypeClasses #-}

module Examples.Factorial where

import Recursive
import Data.Id

--------------------
-- Factorial
--------------------
instance BaseOf Int where
  isBasal n = n == 0

instance Recursive Id Int where
  recurse x
    | x >= 1    = Id $ x - 1
    | otherwise = Id x

factBase :: Int -> Int
factBase 0 = 1
factBase _ = error "Not a base element"

factStep :: Int -> Id Int -> Int
factStep n (Id x) = n*x

fact :: Int -> Int
fact = assemble factStep factBase
