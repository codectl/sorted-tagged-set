module SortedTaggedSetSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import SortedTaggedSet

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "empty" $ do
    it "creates empty set with 0 elements" $ do
      lengthSet empty `shouldBe` 0

  describe "belongs" $ do
    it "is false if set is empty" $ do
      belongs 1 empty `shouldBe` False

    it "is true if element is present" $ do
      belongs 1 (singleton 1) `shouldBe` True

  describe "singleton" $ do
    it "creates set with 1 element" $ do
      lengthSet (singleton 1) `shouldBe` 1

  describe "insertSet" $ do
    it "adds one element to the set if it does not exist" $ do
      lengthSet (insertSet 1 empty) `shouldBe` 1

    it "does not add element if it already exists" $ do
      lengthSet (insertSet 1 $ singleton 1) `shouldBe` 1

  describe "removeSet" $ do
    it "removes element from the set if it exists" $ do
      lengthSet (removeSet 1 $ singleton 1) `shouldBe` 0

    it "does not remove element from the set if it does not exist" $ do
      lengthSet (removeSet 2 $ singleton 1) `shouldBe` 1

  describe "insertTag" $ do
    it "adds tag to element if it exists" $ do
      (peek 1 $ insertTag "a" 1 $ singleton 1) `shouldBe` ["a"]

    it "does not add tag to element if it does not exist" $ do
      (peek 1 $ insertTag "a" 2 $ singleton 1) `shouldBe` []
      (peek 2 $ insertTag "a" 2 $ singleton 1) `shouldBe` []

  describe "merge" $ do
    it "combines two sets with no repeated elements nor tags" $ do
      (lengthSet $ merge (singleton 1) $ merge (singleton 1) (singleton 2)) `shouldBe` 2