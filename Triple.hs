{-# LANGUAGE DeriveGeneric #-}

module Triple where

import Control.Monad
import Test.QuickCheck.Arbitrary
import GHC.Generics (Generic)
import Generic.Random

import Data.Group
import Data.Monoid

data Triple a = Triple{first :: a, second :: a, third :: a}
              deriving (Show, Eq, Generic)

instance Arbitrary a => Arbitrary (Triple a) where
  arbitrary = genericArbitrary uniform

instance Functor Triple where
  fmap f t = Triple (f $ first t) (f $ second t) (f $ third t)

-- Reorder permutations
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

toCycle :: Triorder -> Cycle
toCycle order =
  case order of
       (Triple 0 1 2) -> EmptyCycle
       (Triple 0 2 1) -> Twocycle B C 
       (Triple 1 0 2) -> Twocycle A B
       (Triple 1 2 0) -> Tricycle A B C
       (Triple 2 0 1) -> Tricycle A C B
       (Triple 2 1 0) -> Twocycle A C
       _              -> error $ "Triorder " ++ show order ++ " is not valid."

-- Cycle permuattions
data Index3 = A | B | C
            deriving (Eq,Show)

fromIndex :: Index3 -> Int
fromIndex index =
  case index of
       A -> 0
       B -> 1
       C -> 2

data Cycle = EmptyCycle
           | Twocycle Index3 Index3
           | Tricycle Index3 Index3 Index3
           deriving (Eq, Show)
-- TODO define own Eq e.g. Tricycle A B C == Tricycle B C A == Tricycle C A B

-- TODO define Monoid instance
instance Group Cycle where
  invert cycle =
    case cycle of
         EmptyCycle     -> EmptyCycle
         Twocycle p q   -> Twocycle p q
         Tricycle p q r -> Tricycle r q p

reverseCycle :: Cycle -> Cycle
reverseCycle cycle =
  case cycle of
       EmptyCycle -> EmptyCycle
data Fallible a = Suc a | Err String

instance Monad Fallible where
  return = Suc
  fx >>= f = case fx of
                  Suc x -> f x
                  Err s -> Err s

instance Applicative Fallible where
  pure = return
  (<*>) = Control.Monad.ap

instance Functor Fallible where
  fmap = Control.Monad.liftM

isHetero :: (Eq a) => [a] -> Bool
-- \xs -> exists x,y:Subset xs. x /= y
isHetero [] = True
isHetero (x:xs) = notElem x xs && isHetero xs

isHomo :: (Eq a) => [a] -> Bool
isHomo [] = True
isHomo (x:xs) = filter (==x) xs == xs

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x:removeDuplicates (filter (/= x) xs)

isInRange :: Int -> Int -> Int -> Bool
isInRange p q x = x `elem` [p..q]

isCycleValid :: Cycle -> Fallible Cycle
isCycleValid cycle =
  case cycle of
       Tricycle p q r -> if isHetero [p,q,r] then Suc cycle else Err errMsg
       Twocycle p q   -> if isHetero [p,q]   then Suc cycle else Err errMsg
       EmptyCycle     -> Suc cycle
    where errMsg = "Invalid Cycle:" ++ show cycle
  
cycleTripleUnsafely :: Cycle -> Triple a -> Triple a
cycleTripleUnsafely cycle triple = 
  case cycle of
       EmptyCycle -> triple
       Twocycle p q   -> transposeTriple p' q' triple
         where [p',q'] = fmap fromIndex [p,q]
       Tricycle p q r -> (transposeTriple p' q' . transposeTriple q' r') triple
         where [p',q',r'] = fmap fromIndex [p,q,r]

cycleTriple :: Cycle -> Triple a -> Fallible (Triple a)
cycleTriple cycle triple =
  fmap (`cycleTripleUnsafely` triple) (isCycleValid cycle)

decomposeCycleUnsafely :: Cycle -> [(Int,Int)]
decomposeCycleUnsafely cycle =
  case cycle of
     EmptyCycle -> []
     Twocycle p q   -> [(p',q')]
         where [p',q'] = fmap fromIndex [p,q]
     Tricycle p q r -> [(p',q'),(q',r')]
         where [p',q',r'] = fmap fromIndex [p,q,r]

decomposeCycle :: Cycle -> Fallible [(Int,Int)]
decomposeCycle cycle =
  fmap decomposeCycleUnsafely (isCycleValid cycle)
 
-- Transposition permutations
transposeTriple :: Int -> Int -> Triple a -> Triple a
transposeTriple p q triple@(Triple a b c) =
  if isHetero [p,q] && all (isInRange 0 2) [p,q]
     then case [p,q] of
               [0,1] -> Triple b a c
               [0,2] -> Triple c b a
               [1,2] -> Triple a c b
               [x,y] -> transposeTriple y x (Triple a b c)
     else error $ "transposeTriple " ++ show p ++ " " ++ show q ++ " - Invalid transposition positions"
