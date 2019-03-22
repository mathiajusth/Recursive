{-# LANGUAGE DeriveGeneric #-}

module Triple where

import Control.Monad
import Test.QuickCheck.Arbitrary
import GHC.Generics (Generic)
import Generic.Random

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

-- Cycle permuattions
type Tricycle = [Int]

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

isTricycleValid :: Tricycle -> Fallible Tricycle
isTricycleValid cycle = let size = length cycle
  in if (size == 2 || size == 3) && isHetero cycle && all (isInRange 0 2) cycle
        then Suc cycle
        else Err $ "Invalid Tricycle:" ++ show cycle

cycleTripleUnsafely :: Tricycle -> Triple a -> Triple a
cycleTripleUnsafely cycle triple = 
  case cycle of
       [p,q] -> transposeTriple p q triple
       [p,q,r] -> (transposeTriple p q . transposeTriple q r) triple

cycleTriple :: Tricycle -> Triple a -> Fallible (Triple a)
cycleTriple cycle triple =
  fmap (flip cycleTripleUnsafely triple) (isTricycleValid cycle)

decomposeCycleUnsafely :: Tricycle -> [(Int,Int)]
decomposeCycleUnsafely cycle =
  case cycle of
     [p,q] -> [(p,q)]
     [p,q,r] -> [(p,q),(q,r)]

decomposeCycle :: Tricycle -> Fallible [(Int,Int)]
decomposeCycle cycle =
  fmap decomposeCycleUnsafely (isTricycleValid cycle)
 
-- Transposition permutations
transposeTriple :: Int -> Int -> Triple a -> Triple a
transposeTriple p q (Triple a b c) =
  if isHetero [p,q] && all (isInRange 0 2) [p,q]
     then case [p,q] of
               [0,1] -> Triple b a c
               [0,2] -> Triple c b a
               [1,2] -> Triple a c b
               [x,y] -> transposeTriple y x (Triple a b c)
     else error $ "Invalid transposition positions: " ++ show [p,q]


--edecomposeTriorder :: Triorder -> [Transposition]

-- 2 0 1
-- 1 2 0
-- a b c ~> c a b ~> a b c
-- 0 1 2 to 2 0 1 is (0 2 1)
-- reverse (0 2 1) is (1 2 0)

-- hypothesis (0 2 1) = (0 2) . (2 1)
-- a b c ~> c b a ~> c a b ~> a c b

-- 0 2 1
-- 0 2 1
-- a b c ~> a c b ~> a b c
-- 0 1 2 to 0 2 1 is (1 2)
-- reverse (1 2) is (2 1)


-- transformTriple :: Triple a -> (a, a, a)
-- transformTriple (Triple a b c) = (a, b ,c)

