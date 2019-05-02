{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Examples.TowerOfHanoiOptimized where

import Recursive
import Data.State
import Utils.Miscellaneous(transposeValues)
import Examples.TowerOfHanoi(Height, Steps, Pole(..))

import Data.Tuple.Extra(both)

--------------------
-- Tower of Hanoi Optimized
--------------------
data AbstractPole = From | To | Other
                  deriving (Eq, Show)

instance BaseOf Height where
  isBasal h = h== 0 || h == 1

instance Recursive [] Height where
  recurse h = [h - 1 ,1]

type AbstractStep = (AbstractPole,AbstractPole)
type AbstractSteps = [AbstractStep]

exchangePoles :: (AbstractPole,AbstractPole) -> AbstractSteps -> AbstractSteps
exchangePoles cycle  = fmap (both $ transposeValues cycle)

type SubstitutionRule = (AbstractPole,AbstractPole)
type SubstitutionRules = [SubstitutionRule]

hanoiBaseO :: Height -> AbstractSteps
hanoiBaseO 0 =  []
hanoiBaseO 1 =  [(From,To)]

hanoiStepO :: Height -> [AbstractSteps] -> AbstractSteps
hanoiStepO _ [moveTopSteps, moveBottomSteps] = concat
  [exchangePoles (To  ,Other) moveTopSteps 
  ,                           moveBottomSteps 
  ,exchangePoles (From,Other) moveTopSteps
  ]

tohO :: Height -> AbstractSteps
tohO = assemble hanoiStepO hanoiBaseO

tohOC :: Height -> State Count AbstractSteps
tohOC = assembleWithCount hanoiStepO hanoiBaseO

--------------------
-- TESTING
--------------------

toConcreteSteps :: AbstractSteps -> Steps
toConcreteSteps = fmap toConcreteStep
  where toConcreteStep (p,q) = (toConcretePole p,toConcretePole q)
        toConcretePole p = case p of
                                From  -> First
                                To    -> Third
                                Other -> Second
