{-# LANGUAGE ScopedTypeVariables #-}

import Recursive
import Triple

import Test.Hspec
import Test.QuickCheck
import Control.Exception

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

  describe "solveHT" $ do
    it "solves correctly" $ property $
      \ (h :: Int) ->
        h >= 0 && h <= 10 ==>
          moveBricks (solveHT h) (buildTowers h 0 0) == buildTowers 0 0 h
