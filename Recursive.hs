{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Recursive where

import Triple

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

-- TESTING

type Brick = Int
type Tower = [Brick]
type HanoiTowers = Triple Tower

initHT :: Int -> Triple Tower
initHT n = Triple {first = [1..n], second = [], third = []}

makeHT :: Tower -> Tower -> Tower -> HanoiTowers
makeHT t1 t2 t3 =
  Triple {first = t1
         ,second = t2
         ,third = t3
         }

switchTowers :: Pole -> Pole -> HanoiTowers -> HanoiTowers
switchTowers p1 p2 ht 
  | p1 == p2 = ht
  | otherwise =
      case (p1, p2) of
           (First, Second) -> ht {first = second ht, second = first ht}
           (Second, Third) -> ht {second = third ht, third = second ht}
           (Third, First)  -> ht {first = third ht, third = first ht}
           (x, y) -> switchTowers y x ht

choseTower :: Pole -> HanoiTowers -> Tower
choseTower pole ht =
  case pole of
       First -> first ht
       Second -> second ht
       Third -> third ht

moveBrick :: Pole -> Pole -> HanoiTowers -> HanoiTowers
moveBrick p1 p2 ht =
  let (moveFromT, moveToT) = (choseTower p1 ht, choseTower p2 ht)
      in case (moveFromT, moveToT) of
              ([], _) ->
                error $ "there is no brick to move from '" ++ show p1 ++ "' pole"
              (brick1:_, brick2:_) ->
                if brick1 < brick2
                   then unsafelyMoveBrick p1 p2 ht
                   else error "Cannot move a bigger brick onto a smaller one"
              (_,_) -> unsafelyMoveBrick p1 p2 ht

unsafelyMoveBrick :: Pole -> Pole -> HanoiTowers -> HanoiTowers
unsafelyMoveBrick First Second ht@Triple{first = (b:bs), second} =
  ht {first = bs
     ,second = b:second
     }
unsafelyMoveBrick Second First ht =
  (switchTowers First Second 
  .unsafelyMoveBrick First Second 
  .switchTowers First Second
  ) ht
unsafelyMoveBrick p1 p2 ht =
  (switchTowers Second p2 . switchTowers First p1
  .unsafelyMoveBrick First Second
  .switchTowers First p1 . switchTowers Second p2
  ) ht

moveBricks :: Steps -> HanoiTowers -> HanoiTowers
moveBricks steps ht = foldl (flip $ uncurry moveBrick) ht steps
