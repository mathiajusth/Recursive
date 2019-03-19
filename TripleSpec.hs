import Recursive
import Test.Hspec
import Test.QuickCheck

-- prop_identityOf012 :: Triple Int -> Bool
-- prop_identityOf012 triple = reorderTriple (Triple 0 1 2) triple == triple

main :: IO ()
main = hspec $ do
  describe "Triple.reoderTriple" $ do
    it "is identity for order (Triple 0 1 2)" $ do
      property $
        \triple ->
          reorderTriple (Triple 0 1 2) triple == (triple :: Triple Int)

    it "switches first two for order (Triple 1 0 2)" $ do
      property $
        \triple ->
          let (Triple a b c) = triple :: Triple Int
              in reorderTriple (Triple 1 0 2) triple == Triple b a c

  describe "isHetero" $ do
    it "is True for []" $ do
      isHetero ([] :: [Int]) `shouldBe` True

    it "is True for [1]" $ do
      isHetero [1] `shouldBe` True

    it "is True for [1,2]" $ do
      isHetero [1,2] `shouldBe` True

    it "is True false [1,2]" $ do
      isHetero [1,1] `shouldBe` False

    it "is False for replicated values" $ do
      property $ prop
        where prop :: Int -> Char -> Property
              prop m x =
                m > 1 ==>
                      let xs = replicate m x
                          in not $ isHetero xs
