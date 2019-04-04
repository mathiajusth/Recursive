{-# LANGUAGE ScopedTypeVariables #-}

import Utils.List

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "List.isHetero" $ do
    it "[] ~> True" $
      isHetero ([] :: [Int]) `shouldBe` True

    it "[1] ~> True " $
      isHetero [1] `shouldBe` True

    it "[1,2] ~> True " $
      isHetero [1,2] `shouldBe` True

    it "[1,2] ~> False " $
      isHetero [1,1] `shouldBe` False

    it "replicated values ~> False" $ property $
      \(m::Int) (x::Char) ->
        m > 1 ==>
          let xs = replicate m x
              in not $ isHetero xs

    it "removed duplicates ~> True" $ property $
      \(xs::[Int]) ->
        isHetero $ removeDuplicates xs

  describe "List.isHomo" $ do
    it "[] ~> True " $
      isHomo ([] :: [Int]) `shouldBe` True

    it "[1] ~> True " $
      isHomo [1] `shouldBe` True

    it "[1,2] ~> False " $
      isHomo [1,2] `shouldBe` False

    it "[1,2] ~> True" $
      isHomo [1,1] `shouldBe` True

    it "replicated values ~> True" $ property $ 
      \(m::Int) (x::Char) ->
        let xs = replicate m x
            in isHomo xs

  describe "List.removeDuplicates" $ do
    it "[x,x] ~> [x]" $ property $
      \x -> removeDuplicates [x,x] == ([x] :: [Int])

    it "[x,x,y,y] ~> [x,y]" $ property $
      \(x::Int) (y::Int) ->
        x /= y ==>
          removeDuplicates [x,x,y,y] == [x,y]

    it "keeps first occurences: [x,y,y,x] ~> [x,y]" $ property $
      \(x::Int) (y::Int) ->
        x /= y ==>
          removeDuplicates [x,y,y,x] == [x,y]

    it "IDEMPOTENCE" $ property $
      \(xs::[Int]) ->
        (removeDuplicates . removeDuplicates) xs == removeDuplicates xs

