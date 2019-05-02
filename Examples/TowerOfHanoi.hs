{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Examples.TowerOfHanoi where

import Recursive
import Data.State
import Data.Triple
import Data.Group (invert)

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

instance BaseOf HanoiTask where
  isBasal HanoiTask{height} = height == 0 || height == 1

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

toh :: Height -> Steps
toh h = assemble hanoiStep hanoiBase $ HanoiTask{from = First, to = Third, height = h}

tohC :: Height -> State Count Steps
tohC h = assembleWithCount hanoiStep hanoiBase $ HanoiTask{from = First, to = Third, height = h}

--------------------
-- TESTING
--------------------

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
unsafelyMoveBrick p q ht =
  let [p',q',r'] = fmap toPosition [p,q,otherPole p q]
      ordering = Triple p' q' r'
      in (reorderTriple (invert ordering)
         .unsafelyMoveBrick First Second
         .reorderTriple ordering
         ) ht

moveBricks :: Steps -> HanoiTowers -> HanoiTowers
moveBricks steps ht = foldl (flip $ uncurry moveBrick) ht steps
