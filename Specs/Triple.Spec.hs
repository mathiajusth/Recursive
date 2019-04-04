{-# LANGUAGE ScopedTypeVariables #-}

import Data.Group
import Test.Hspec
import Test.QuickCheck

import Data.Triple
import Utils.List (isHetero)


main :: IO ()
main = hspec $ do
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
      \ (triple :: Triple Int) (p::Int) (q::Int) ->
          let [p',q'] = fmap ((`mod` 3) . abs) [p,q]
              in p' /= q' ==>
                  transposeTriple p' q' triple == transposeTriple q' p' triple

    it "SELF-INVERSE" $ property $
      \ (triple :: Triple Int) (p::Int) (q::Int) ->
          let [p',q'] = fmap ((`mod` 3) . abs) [p,q]
              in p' /= q' ==>
                (transposeTriple p' q' . transposeTriple p' q') triple == triple

    it "transposeTriple 0 1 (Triple a b c) == Triple b a c" $ property $
      \ (Triple a b c::Triple Int) ->
        transposeTriple 0 1 (Triple a b c) == Triple b a c

    it "transposeTriple 0 2 (Triple a b c) == Triple c b a" $ property $
      \ (Triple a b c::Triple Int) ->
        transposeTriple 0 2 (Triple a b c) == Triple c b a

    it "transposeTriple 1 2 (Triple a b c) == Triple a c b" $ property $
      \ (Triple a b c::Triple Int) ->
        transposeTriple 1 2 (Triple a b c) == Triple a c b


  describe "Triple.invert (Data.Group instance)" $ do
    it "invert Tricycle p q r ~> Tricycle r q p" $ property $
      \ (triple :: Triple Int) (p::Int) (q::Int) (r::Int) ->
        let [p',q',r'] = fmap ((`mod` 3) . abs) [p,q,r]
            cycle = Tricycle p' q' r' -- TODO abstract to Arbitrary
            in p' /= q' && q' /= r' ==>
              (cycleTriple cycle . cycleTriple (invert cycle)) triple ==  triple

  describe "Triple.cycleTriple" $ do
    it "(Tricycle 0 1 2) (Triple a b c) ~> Triple c a b" $ property $
      \ (Triple a b c::Triple Int) ->
        cycleTriple (Tricycle 0 1 2) (Triple a b c) == Triple c a b

  describe "Triple.cycleToOrder" $ do
    it "transforms correctly" $ property $
      \ (triple :: Triple Int) (p::Int) (q::Int) (r::Int) ->
        let [p',q',r'] = fmap ((`mod` 3) . abs) [p,q,r]
            cycle = Tricycle p' q' r' -- TODO abstract to Arbitrary
            in p' /= q' && q' /= r' ==>
              cycleTriple cycle triple == reorderTriple (cycleToOrder cycle)  triple

  describe "Triple.orderToCycle" $ do
    it "transforms correctly" $ property $
      \ (triple :: Triple Int) (p::Int) (q::Int) (r::Int) ->
        let [p',q',r'] = fmap ((`mod` 3) . abs) [p,q,r]
            order = Triple p' q' r' -- TODO abstract to Arbitrary
            in isHetero [p',q',r'] ==>
              reorderTriple order triple == cycleTriple (orderToCycle order) triple
