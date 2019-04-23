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

newtype PFNat = PFNat Int

instance WithSubset PFNat where
  isInSubset (PFNat n) = isPrime n

instance Recursive Id PFNat where
  recurse (PFNat n) = Id . PFNat $ div n (lowestDivisor n)

lowestDivisor :: Int -> Int
lowestDivisor n =
  let maybeDivisor = find (\i ->mod n i == 0) [2..n-1]
      in fromMaybe n maybeDivisor

primeFactorCountBase :: PFNat -> Int
primeFactorCountBase _ = 1

primeFactorCountStep :: PFNat -> Id Int -> Int
primeFactorCountStep _ (Id count) = 1 + count

primeFactorCount :: Int -> Int
primeFactorCount n = assemble primeFactorCountStep primeFactorCountBase $ PFNat n
