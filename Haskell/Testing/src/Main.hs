-- Hspec
-- A Testing Framework for Haskell
-- https://hspec.github.io/
import Test.Hspec

-- QuickCheck
-- Automatic testing of Haskell programs
-- http://hackage.haskell.org/package/QuickCheck
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d     = (count, n)
          | otherwise = go (n - d) d (count + 1)

-- Hspec unit tests are grouped together by a common description and context.

main :: IO ()
main = hspec $ do

  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4

    -- QuickCheck is used here to do a property test.
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

    context "with negatives" $ do
      it "2 + (-2) is equal to 0" $ do
        2 + (-2) `shouldBe` 0

  describe "dividedBy" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
