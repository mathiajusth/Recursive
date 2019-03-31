{-# LANGUAGE DeriveGeneric #-}

module Triple where

import Control.Monad
import Test.QuickCheck.Arbitrary
import GHC.Generics (Generic)
import Generic.Random

import MyList (isInRange)
import Data.Group
import Data.Monoid

data Triple a = Triple{first :: a, second :: a, third :: a}
              deriving (Show, Eq, Generic)

instance Arbitrary a => Arbitrary (Triple a) where
  arbitrary = genericArbitrary uniform

instance Functor Triple where
  fmap f t = Triple (f $ first t) (f $ second t) (f $ third t)

--------------------------
-- Reorder permutations
--------------------------
type Triorder = Triple Int

reorderTriple :: Triorder -> Triple a -> Triple a
reorderTriple order triple@(Triple a b c) =
     case order of
       (Triple 0 1 2) -> triple
       (Triple 0 2 1) -> Triple a c b 
       (Triple 1 0 2) -> Triple b a c
       (Triple 1 2 0) -> Triple b c a
       (Triple 2 0 1) -> Triple c a b
       (Triple 2 1 0) -> Triple c b a
       _       -> error $ "Triorder " ++ show order ++ " is not valid."

orderToCycle :: Triorder -> Cycle
orderToCycle order =
  case order of
       (Triple 0 1 2) -> EmptyCycle
       (Triple 0 2 1) -> Twocycle 1 2 
       (Triple 1 0 2) -> Twocycle 0 1
       (Triple 1 2 0) -> Tricycle 0 2 1
       (Triple 2 0 1) -> Tricycle 0 1 2
       (Triple 2 1 0) -> Twocycle 0 2
       _              -> error $ "Triorder " ++ show order ++ " is not valid."

-- decomposeOrderToTranspositions :: Triorder -> [(Int,Int)]
-- TODO HOLE

--------------------------
-- Cycle permuattions
--------------------------
data Cycle = EmptyCycle
           | Twocycle Int Int
           | Tricycle Int Int Int
           deriving (Eq, Show)
-- TODO define own Eq e.g. Tricycle A B C == Tricycle B C A == Tricycle C A B

-- TODO define Monoid instance
instance Group Cycle where
  invert cycle =
    case cycle of
         EmptyCycle     -> EmptyCycle
         Twocycle p q   -> Twocycle q p
         Tricycle p q r -> Tricycle r q p

instance Monoid Cycle where
  mempty = EmptyCycle
  mappend EmptyCycle cycle = cycle
  mappend cycle EmptyCycle = cycle
  mappend _ _ = EmptyCycle -- TODO HOLE

-- data Fallible a = Suc a | Err String

-- instance Monad Fallible where
--   return = Suc
--   fx >>= f = case fx of
--                   Suc x -> f x
--                   Err s -> Err s

-- instance Applicative Fallible where
--   pure = return
--   (<*>) = Control.Monad.ap

-- instance Functor Fallible where
--   fmap = Control.Monad.liftM

  
cycleTriple:: Cycle -> Triple a -> Triple a
cycleTriple cycle triple = 
  case cycle of
       EmptyCycle     -> triple
       Twocycle p q   -> transposeTriple p q triple
       Tricycle p q r -> (transposeTriple p q . transposeTriple q r) triple

-- cycleTriple :: Cycle -> Triple a -> Triple a
-- cycleTriple cycle triple =
--   fmap (`cycleTripleUnsafely` triple) (isCycleValid cycle)

decomposeCycleToPermutations:: Cycle -> [(Int,Int)]
decomposeCycleToPermutations cycle =
  case cycle of
     EmptyCycle -> []
     Twocycle p q   -> [(p,q)]
     Tricycle p q r -> [(p,q),(q,r)]

cycleToOrder :: Cycle -> Triorder
cycleToOrder EmptyCycle = Triple 0 1 2
cycleToOrder (Twocycle p q) = transposeTriple p q (Triple 0 1 2) 
cycleToOrder (Tricycle p q r) = (transposeTriple p q . transposeTriple q r) (Triple 0 1 2)

--------------------------
-- Transposition permutations
--------------------------
transposeTriple :: Int -> Int -> Triple a -> Triple a
transposeTriple p q triple@(Triple a b c) =
  if all (isInRange 0 2) [p,q]
     then case [p,q] of
               [0,1] -> Triple b a c
               [0,2] -> Triple c b a
               [1,2] -> Triple a c b
               [0,0] -> triple
               [1,1] -> triple
               [2,2] -> triple
               [x,y] -> transposeTriple y x (Triple a b c)
     else error $ "transposeTriple " ++ show p ++ " " ++ show q ++ " - Invalid transposition positions"
