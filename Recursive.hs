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

isTripleOrdering :: (Int, Int, Int)-> Bool
isTripleOrdering (p1, p2, p3) = let isPosition x = x == 0 || x== 1 || x == 2
                 in and $ fmap isPosition [p1, p2, p3]

reorder :: (Int, Int, Int) -> Triple a -> Triple a
reorder order@(p1, p2, p3) triple@(Triple fst snd trd)
  | isTripleOrdering order =
      case (p1, p2, p3) of
           (0,1,2) -> triple
           (0,2,1) -> Triple fst trd snd
           (1,0,2) -> Triple snd fst trd
           (1,2,0) -> Triple snd trd fst
           (2,0,1) -> Triple trd fst snd
           (2,1,0) -> Triple trd snd fst
 | otherwise =
     error $ "Invalid reordering: " ++
             show p1 ++ show p2 ++ show p3 ++
             ". It needs to contain three different numbers in the range from 0 to 2"

transformTriple :: Triple a -> (a, a, a)
transformTriple (Triple a b c) = (a, b ,c)

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
type Towers = Triple Tower

switchTowers :: Pole -> Pole -> Towers -> Towers
switchTowers p1 p2 ht 
  | p1 == p2 = ht
  | otherwise =
      case (p1, p2) of
           (First, Second) -> ht {first = second ht, second = first ht}
           (Second, Third) -> ht {second = third ht, third = second ht}
           (Third, First)  -> ht {second = third ht, third = second ht}
           (p1,p2) -> switchTowers p2 p1 ht

reorderTowers :: Triple Pole -> Towers -> Towers
reorderTowers poleOrder = reorder $ transformTriple (fmap poleToPosition poleOrder)
  where poleToPosition :: Pole -> Int
        poleToPosition First = 0
        poleToPosition Second = 1
        poleToPosition Third = 2

choseTower :: Pole -> Towers -> Tower
choseTower pole ht =
  case pole of
       First -> first ht
       Second -> second ht
       Third -> third ht

moveBrick :: Pole -> Pole -> Towers -> Towers
moveBrick p1 p2 ts =
  let (moveFromT, moveToT) = (choseTower p1 ts, choseTower p2 ts)
      in case (moveFromT, moveToT) of
              ([], _) ->
                error $ "there is no brick to move from '" ++ show p1 ++ "' pole"
              (brick1:t1, brick2:t2) ->
                if brick1 < brick2
                   then trulyMoveBrick p1 p2 ts
                   else error "Cannot move a bigger brick onto a smaller one"
              (_,_) -> trulyMoveBrick p1 p2 ts
            where trulyMoveBrick First Second ts =
                    ts {first = tail $ first ts, second = head (first ts):second ts}
                  trulyMoveBrick p1 p2 ts =
                    (reorderTowers inverseOrder
                    .trulyMoveBrick First Second
                    .reorderTowers order
                    ) ts
                    where order = Triple p1 p2 (otherPole p1 p2)
                          inverseOrder = 
