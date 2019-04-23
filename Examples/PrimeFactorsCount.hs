{-# LANGUAGE MultiParamTypeClasses #-}

module Examples.Factorial where

import Data.Numbers.Primes(isPrime)
import GHC.OldList(find)
import Data.Maybe(fromMaybe)

import Recursive
import Data.Id

--------------------
-- Prime Factors Count
--------------------

instance BaseOf Int where
  isBasal = isPrime

instance Recursive Id Int where
  recurse n = Id $ div n (lowestDivisor n)

lowestDivisor :: Int -> Int
lowestDivisor n =
  let maybeDivisor = find (\i ->mod n i == 0) [2..n-1]
      in fromMaybe n maybeDivisor

primeFactorCountBase :: Int -> Int
primeFactorCountBase _ = 1

primeFactorCountStep :: Int -> Id Int -> Int
primeFactorCountStep _ (Id count) = 1 + count

primeFactorCount :: Int -> Int
primeFactorCount = assemble primeFactorCountStep primeFactorCountBase
