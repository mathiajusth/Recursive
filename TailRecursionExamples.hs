module TailRecursionExamples where

import Examples.TowerOfHanoi(Height)
import Examples.TowerOfHanoiOptimized(AbstractSteps, AbstractPole(..), exchangePoles)
import Data.Numbers.Primes(isPrime)

sumNat :: Int -> Int
sumNat n = sumNat' n 0
  where sumNat' 0 acc = acc
        sumNat' m acc = sumNat' (m-1) (acc+m)

fact :: Int -> Int
fact n = fact' n 1
  where fact' 0 acc = acc
        fact' m acc = fact' (m-1) (m*acc)

fib :: Int -> Int
fib n = fib' n 0 1
  where fib' 0 a _ = a
        fib' 1 _ b = b
        fib' m a b = fib' (m-1) b (a+b)

-- nonTail toh
toh :: Height -> AbstractSteps
toh 0 = []
toh 1 = [(From,To)]
toh h = let moveTop    = toh (h-1)
            moveBottom = toh 1
          in concat [exchangePoles (To  ,Other) moveTop
                    ,                           moveBottom
                    ,exchangePoles (From,Other) moveTop]

-- tail toh
tohT :: Height -> AbstractSteps
tohT h = tohT' h [(From,To)] []
  where tohT' 0 _ acc = acc
        tohT' h move1FromTo acc = tohT' (h-1) move1FromTo newAcc
          where newAcc = concat [exchangePoles (To  ,Other) acc
                                ,move1FromTo
                                ,exchangePoles (From,Other) acc]
