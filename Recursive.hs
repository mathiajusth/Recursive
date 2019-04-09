{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Recursive where

import Data.Group (invert)

import Data.Triple
import Data.State
import Data.StateWithLogging

class WithSubset r where
  isInSubset :: r -> Bool

class (Functor m, WithSubset r) => Recursive m r where
  recurse :: Functor m => r -> m r

assemble :: (Recursive m a, WithSubset a) => (a -> m b -> b) -> (a -> b) -> a -> b
assemble step base arg =
  if isInSubset arg
     then base arg
     else step arg $ fmap (assemble step base) (recurse arg)

type Count = Int

-- assemble and count the number of recursive calls
assembleWithCount :: (Recursive m a, WithSubset a, Foldable m) => (a -> m b -> b) -> (a -> b) -> a -> State Count b
assembleWithCount step base arg =
  let stepS arg msb = State $ \s -> (combine . fmap (flip effect s)) msb
        where combine mcb = ((+1).sum $ fmap fst mcb
                            ,step arg $ fmap snd mcb
                            )
      baseS arg = State $ \s -> (s, base arg)
      in assemble stepS baseS arg

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

fibC :: Integer -> State Count Integer
fibC n = assembleWithCount fibStep fibBase $ FibNat n

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
  isInSubset HanoiTask{height} = height == 0 || height == 1

instance Recursive Triple HanoiTask where
  recurse HanoiTask{from, to, height} =
    Triple (HanoiTask from  other (height - 1))
           (HanoiTask from  to    1           )
           (HanoiTask other to    (height - 1))
        where other = otherPole from to

type Steps = [(Pole,Pole)]
hanoiBase :: HanoiTask -> Steps
hanoiBase HanoiTask{from, to, height}
  | height == 0 = []
  | height == 1 = [(from, to)]
  | otherwise = error "Not a base element"

hanoiStep :: HanoiTask -> Triple Steps -> Steps
hanoiStep _ (Triple top bottom topOnBottom) = top ++ bottom ++ topOnBottom

solveHT :: Height -> Steps
solveHT h = assemble hanoiStep hanoiBase $ HanoiTask{from = First, to = Third, height = h}

-- TESTING

type Brick = Int
type Tower = [Brick]
type HanoiTowers = Triple Tower

initHT :: Int -> HanoiTowers
initHT n = Triple [1..n] [] []

buildTowers :: Int -> Int -> Int -> HanoiTowers
buildTowers h1 h2 h3 = Triple [1..h1] [1..h2] [1..h3]

equiwideTowers :: Int -> Int -> Int -> HanoiTowers
equiwideTowers h1 h2 h3 =
  let ones = repeat 1
    in Triple (take h1 ones) (take h2 ones) (take h3 ones)

choseTower :: Pole -> HanoiTowers -> Tower
choseTower pole ht =
  case pole of
       First -> first ht
       Second -> second ht
       Third -> third ht

toPosition :: Pole -> Int
toPosition First = 0
toPosition Second = 1
toPosition Third = 2

moveBrick :: Pole -> Pole -> HanoiTowers -> HanoiTowers
moveBrick p q ht =
  let (moveFromT, moveToT) = (choseTower p ht, choseTower q ht)
      in case (moveFromT, moveToT) of
              ([], _) ->
                error $ "there is no brick to move from '" ++ show p ++ "' pole"
              (brick1:_, brick2:_) ->
                if brick1 <= brick2
                   then unsafelyMoveBrick p q ht
                   else error "Cannot move a bigger brick onto a smaller one"
              (_,_) -> unsafelyMoveBrick p q ht

unsafelyMoveBrick :: Pole -> Pole -> HanoiTowers -> HanoiTowers
unsafelyMoveBrick First Second ht@Triple{first = (b:bs), second} =
  ht {first = bs, second = b:second}
-- unsafelyMoveBrick Second First ht =
--   (transposeTriple 0 1 
--   .unsafelyMoveBrick First Second 
--   .transposeTriple 0 1
--   ) ht
unsafelyMoveBrick p q ht =
  let [p',q',r'] = fmap toPosition [p,q,otherPole p q]
      ordering = Triple p' q' r'
      in (reorderTriple (invert ordering)
         .unsafelyMoveBrick First Second
         .reorderTriple ordering
         ) ht

moveBricks :: Steps -> HanoiTowers -> HanoiTowers
moveBricks steps ht = foldl (flip $ uncurry moveBrick) ht steps
