{-# LANGUAGE ScopedTypeVariables #-}

import Triple

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

  describe "Triple.reorderTriple" $ do
    it "(Triple 0 1 2) is IDENTITY order" $ property $
      \ (triple::Triple Char) ->
        reorderTriple (Triple 0 1 2) triple == triple

    it "(Triple 0 2 1) (Triple a b c) ~> Triple b a c" $ property $
      \ (Triple a b c:: Triple Char) ->
        reorderTriple (Triple 0 2 1) (Triple a b c) == Triple a c b

    it "(Triple 1 0 2) (Triple a b c) ~> Triple b a c" $ property $
      \ (Triple a b c::Triple Char) ->
        reorderTriple (Triple 1 0 2) (Triple a b c) == Triple b a c

    it "(Triple 1 2 0) (Triple a b c) ~> Triple b c a" $ property $
      \ (Triple a b c::Triple Char) ->
        reorderTriple (Triple 1 2 0) (Triple a b c) == Triple b c a

    it "(Triple 2 0 1) (Triple a b c) ~> Triple c a b" $ property $
      \ (Triple a b c::Triple Char) ->
        reorderTriple (Triple 2 0 1) (Triple a b c) == Triple c a b

    it "(Triple 2 1 0) (Triple a b c) ~> Triple c b a" $ property $
      \ (Triple a b c::Triple Char) ->
        reorderTriple (Triple 2 1 0) (Triple a b c) == Triple c b a

  describe "Triple.transposeTriple" $ do
    it "COMMUTATIVITY" $ property $
      \(triple :: Triple Int) (p::Int) (q::Int) ->
          let [p',q'] = fmap ((`mod` 3) . abs) [p,q]
              in p' /= q' ==>
                  transposeTriple p' q' triple == transposeTriple q' p' triple

    it "SELF-INVERSE" $ property $
      \(triple :: Triple Int) (p::Int) (q::Int) ->
          let [p',q'] = fmap ((`mod` 3) . abs) [p,q]
              in p' /= q' ==>
                (transposeTriple p' q' . transposeTriple p' q') triple == triple

    it "transposeTriple 0 1 (Triple a b c) == Triple b a c" $ property $
      \(Triple a b c::Triple Int) ->
        transposeTriple 0 1 (Triple a b c) == Triple b a c

    it "transposeTriple 0 2 (Triple a b c) == Triple c b a" $ property $
      \(Triple a b c::Triple Int) ->
        transposeTriple 0 2 (Triple a b c) == Triple c b a

    it "transposeTriple 1 2 (Triple a b c) == Triple a c b" $ property $
      \(Triple a b c::Triple Int) ->
        transposeTriple 1 2 (Triple a b c) == Triple a c b

  describe "Triple.cycleTripleUnsafely" $ do
    it "reverse cycle == INVERSE of cycle" $ property $
      \(triple :: Triple Int) (p::Int) (q::Int) (r::Int) ->
        let [p',q',r'] = fmap ((`mod` 3) . abs) [p,q,r]
            cycle = [p',q',r']
            in p' /= q' && q' /= r' ==>
              (cycleTripleUnsafely cycle . cycleTripleUnsafely (reverse cycle)) triple ==  triple

    it "[0,1,2] (Triple a b c) ~> Triple c a b" $ property $
      \(Triple a b c::Triple Int) ->
        cycleTripleUnsafely [0,1,2] (Triple a b c) == Triple c a b
