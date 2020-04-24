module DomainSpecs.BoundsSpec where

import           ClassyPrelude
import           Test.Hspec
import           Domain.Revision

spec :: Spec
spec = do
  describe "success" $ it "simple" $ sortedWithoutOverlapsBounds [(1, 2), (3, 4), (5, 8)] `shouldBe` True
  it "empty list is OK" $ sortedWithoutOverlapsBounds [] `shouldBe` True
  it "single element can be ok" $ sortedWithoutOverlapsBounds [(1, 2)] `shouldBe` True
  it "tuples can be of same value" $ sortedWithoutOverlapsBounds [(1, 1), (4, 4), (5, 5)] `shouldBe` True


  describe "failure" $ it "not sorted" $ sortedWithoutOverlapsBounds [(4, 4), (1, 1), (5, 5)] `shouldBe` False
  it "single element can be nok" $ sortedWithoutOverlapsBounds [(3, 2)] `shouldBe` False
  it "invalid bound" $ sortedWithoutOverlapsBounds [(2, 1), (4, 4), (5, 5)] `shouldBe` False
  it "overlaps" $ sortedWithoutOverlapsBounds [(1, 3), (3, 4), (5, 5)] `shouldBe` False




