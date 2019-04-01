import Recursive

import Test.Hspec
import Test.QuickCheck

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

