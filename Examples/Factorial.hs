{-# LANGUAGE MultiParamTypeClasses #-}

module Examples.Factorial where

import Recursive
import Data.Id

--------------------
-- Factorial
--------------------
newtype FactNat = FactNat Int

instance WithSubset FactNat where
  isInSubset (FactNat n) = n == 0

instance Recursive Id FactNat where
  recurse (FactNat x)
    | x >= 1    = Id . FactNat $ x - 1
    | otherwise = Id . FactNat $ x

factBase :: FactNat -> Int
factBase (FactNat 0) = 1
factBase _ = error "Not a base element"

factStep :: FactNat -> Id Int -> Int
factStep (FactNat n) (Id x) = n*x

fact :: Int -> Int
fact n = assemble factStep factBase $ FactNat n
