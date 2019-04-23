{-# LANGUAGE MultiParamTypeClasses #-}

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

newtype TowerHeight = TowerHeight Int

instance WithSubset TowerHeight where
  isInSubset (TowerHeight h) = h== 0 || h == 1

instance Recursive [] TowerHeight where
  recurse (TowerHeight h) = [TowerHeight (h - 1)
                            ,TowerHeight 1
                            ]

type AbstractStep = (AbstractPole,AbstractPole)
type AbstractSteps = [AbstractStep]

exchangePoles :: (AbstractPole,AbstractPole) -> AbstractSteps -> AbstractSteps
exchangePoles cycle  = fmap (both $ transposeValues cycle)

type SubstitutionRule = (AbstractPole,AbstractPole)
type SubstitutionRules = [SubstitutionRule]

hanoiBaseO :: TowerHeight -> AbstractSteps
hanoiBaseO (TowerHeight 0) =  []
hanoiBaseO (TowerHeight 1) =  [(From,To)]

hanoiStepO :: TowerHeight -> [AbstractSteps] -> AbstractSteps
hanoiStepO _ [moveTopSteps, moveBottomSteps] = concat
  [exchangePoles (To  ,Other) moveTopSteps 
  ,                           moveBottomSteps 
  ,exchangePoles (From,Other) moveTopSteps
  ]

tohO :: Height -> AbstractSteps
tohO h = assemble hanoiStepO hanoiBaseO $ TowerHeight h

tohOC :: Height -> State Count AbstractSteps
tohOC h = assembleWithCount hanoiStepO hanoiBaseO $ TowerHeight h 

toConcreteSteps :: AbstractSteps -> Steps
toConcreteSteps = fmap toConcreteStep
  where toConcreteStep (p,q) = (toConcretePole p,toConcretePole q)
        toConcretePole p = case p of
                                From  -> First
                                To    -> Third
                                Other -> Second
