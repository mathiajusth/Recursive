{-# LANGUAGE ScopedTypeVariables #-}

import Recursive(tohO)
import TailRecursionExamples

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "toh" $ do
    it "computes correct steps" $ property $
      \ (h :: Int) ->
        h >= 0 && h <= 11 ==>
          toh h == tohO h

  describe "tohT" $ do
    it "computes correct steps" $ property $
      \ (h :: Int) ->
        h >= 0 && h <= 11 ==>
          tohT h == toh h
