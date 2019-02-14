{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Recursive where

class WithSubset r where
  isInSubset :: r -> Bool

class (Functor m, WithSubset r) => Recursive m r where
  recurse :: Functor m => r -> m r

assemble :: (Recursive m a, WithSubset a) => (a -> m b -> b) -> (a -> b) -> a -> b
assemble step base x =
  if isInSubset x
     then base x
     else step x $ fmap (assemble step base) (recurse x)

--------------------
   -- EXAMPLES --
--------------------

--------------------
-- Factorial
--------------------
newtype FactNat = FactNat Integer

instance WithSubset FactNat where
  isInSubset (FactNat n) = n == 0

newtype Id a = Id a

instance Functor Id where
  fmap f  (Id a) = Id $ f a

instance Recursive Id FactNat where
  recurse (FactNat x)
    | x >= 1    = Id . FactNat $ x - 1
    | otherwise = Id . FactNat $ x

factBase :: FactNat -> Integer
factBase (FactNat 0) = 1
factBase _ = error "Not a base element"

factStep :: FactNat -> Id Integer -> Integer
factStep (FactNat n) (Id x) = n*x

fact :: Integer -> Integer
fact n = assemble factStep factBase $ FactNat n

--------------------
-- Fibonacci
--------------------
newtype FibNat = FibNat Integer

instance WithSubset FibNat where
  isInSubset (FibNat x) = x == 0 || x == 1

instance Recursive [] FibNat where
  recurse (FibNat x)
    | x >= 2    = [FibNat $ x - 2, FibNat $ x - 1]
    | otherwise = return . FibNat $ x

fibBase :: FibNat -> Integer
fibBase (FibNat 0) = 0
fibBase (FibNat 1) = 1
fibBase _ = error "Not a base element"

fibStep :: FibNat -> [Integer] -> Integer
fibStep _ = sum

fib :: Integer -> Integer
fib n = assemble fibStep fibBase $ FibNat n

--------------------
-- Tower of Hanoi
--------------------
data Pole = First | Second | Third
          deriving (Show, Eq)
otherPole :: Pole -> Pole -> Pole
otherPole First Second = Third
otherPole First Third = Second
otherPole Second Third = First
otherPole p q = otherPole q p

type Height = Int
data HanoiTask = HanoiTask
  {from   :: Pole
  ,to     :: Pole
  ,height :: Height
  }

instance WithSubset HanoiTask where
  isInSubset HanoiTask{height = h} = h == 0 || h == 1

data Triple a = Triple{first :: a, second :: a, third :: a}
instance Functor Triple where
  fmap f t = Triple (f $ first t) (f $ second t) (f $ third t)

instance Recursive Triple HanoiTask where
  recurse HanoiTask{from = from, to = to, height = height} =
    let other = otherPole from to
        in Triple (HanoiTask{from = from
                            ,to = other
                            ,height = height - 1
                            }
                  )
                  (HanoiTask{from = from
                            ,to = to
                            ,height = 1
                            }
                  )
                  (HanoiTask{from = other
                            ,to = to
                            ,height = height - 1
                            }
                  )

type Steps = [(Pole,Pole)]
hanoiBase :: HanoiTask -> Steps
hanoiBase HanoiTask{from = from, to = to, height = height}
  | height == 0 = []
  | height == 1 = [(from, to)] 
  | otherwise = error "Not a base element"

hanoiStep :: HanoiTask -> Triple Steps -> Steps
hanoiStep _ (Triple top bottom topOnBottom) = top ++ bottom ++ topOnBottom

toh :: Height -> Steps
toh h = assemble hanoiStep hanoiBase $ HanoiTask{from = First, to = Third, height = h}
