{-# LANGUAGE ScopedTypeVariables #-}

import Data.Triple
import Data.State(returnState)

import Test.Hspec
import Test.QuickCheck
import Control.Exception

import Examples.Fibonacci
import Examples.TowerOfHanoi
import Examples.TowerOfHanoiOptimized

fibComplexity :: Int -> Int
fibComplexity 0 = 0
fibComplexity 1 = 0
fibComplexity n = 1 + fibComplexity (n-1) + fibComplexity (n-2)

tohComplexity :: Height -> Int
tohComplexity 0 = 0
tohComplexity 1 = 0
tohComplexity n = 1 + 2*tohComplexity (n-1)

tohOComplexity :: Height -> Int
tohOComplexity 0 = 0
tohOComplexity 1 = 0
tohOComplexity n = 1 + tohOComplexity (n-1)

main :: IO ()
main = hspec $ do
  describe "unsafelyMoveBrick" $ do
    it "moves from First to Second Pole" $ property $
      unsafelyMoveBrick First Second (equiwideTowers 1 1 1) == equiwideTowers 0 2 1

    it "moves from First to Third Pole" $ property $
      unsafelyMoveBrick First Third (equiwideTowers 1 1 1) == equiwideTowers 0 1 2

    it "moves from Second to Third Pole" $ property $
      unsafelyMoveBrick Second Third (equiwideTowers 1 1 1) == equiwideTowers 1 0 2

    it "moves from Seoncd to First Pole" $ property $
      unsafelyMoveBrick Second First (equiwideTowers 1 1 1) == equiwideTowers 2 0 1

    it "moves from Third to First Pole" $ property $
      unsafelyMoveBrick Third First (equiwideTowers 1 1 1) == equiwideTowers 2 1 0

    it "moves from Third to Second Pole" $ property $
      unsafelyMoveBrick Third Second (equiwideTowers 1 1 1) == equiwideTowers 1 2 0

  describe "moveBrick" $ do
    it "throws error when there is no birck to move" $
      evaluate (moveBrick First Second (equiwideTowers 0 1 1)) `shouldThrow` anyException

    it "throws error when the brick is larger" $
      evaluate (moveBrick First Second (Triple [2] [1] [])) `shouldThrow` anyException

  describe "toh" $ do
    it "solves correctly" $ property $
      \ (h :: Int) ->
        h >= 0 && h <= 10 ==>
          moveBricks (toh h) (buildTowers h 0 0) == buildTowers 0 0 h

  describe "tohO" $ do
    it "computes correct steps" $ property $
      \ (h :: Int) ->
        h >= 0 && h <= 11 ==>
          toConcreteSteps (tohO h) == toh h

  describe "tohC" $ do
    it "counts complexity correctly" $ property $
      \ (n :: Int) ->
        n >= 0 && n <= 11 ==>
          returnState (tohC n) == tohComplexity n

  describe "tohOC" $ do
    it "counts complexity correctly" $ property $
      \ (n :: Int) ->
        n >= 0 && n <= 11 ==>
          returnState (tohOC n) == tohOComplexity n

  describe "fibC" $ do
    it "counts complexity correctly" $ property $
      \ (n :: Int) ->
        n >= 0 && n <= 20 ==>
          returnState (fibC n) == fibComplexity n

