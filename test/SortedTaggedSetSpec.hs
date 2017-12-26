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

  describe "nullSet" $ do
    it "is true when set is empty" $ do
      nullSet empty `shouldBe` True

    it "is false when set is not empty" $ do
      nullSet (singleton 1) `shouldBe` False

  describe "belongs" $ do
    it "is false if set is empty" $ do
      belongs 1 empty `shouldBe` False

    it "is true if element is present" $ do
      belongs 1 (singleton 1) `shouldBe` True
      belongs 2 (insertSet 2 $ singleton 1) `shouldBe` True

  describe "singleton" $ do
    it "creates set with 1 element" $ do
      lengthSet (singleton 1) `shouldBe` 1

  describe "peek" $ do
    it "is empty set if element does not exist" $ do
      peek 2 (singleton 1) `shouldBe` []

    it "gets tag if element exists" $ do
      peek 2 (insertTag "a" 2 $ insertSet 2 $ singleton 1) `shouldBe` ["a"]

  describe "insertSet" $ do
    it "adds one element to the set if it does not exist" $ do
      lengthSet (insertSet 1 empty) `shouldBe` 1
      lengthSet (insertSet 3 $ insertSet 1 $ singleton 2) `shouldBe` 3
      (peek 1 $ insertSet 1 $ singleton 2) `shouldBe` []

    it "does not add element if it already exists" $ do
      lengthSet (insertSet 1 $ singleton 1) `shouldBe` 1
      (peek 1 $ insertSet 1 $ singleton 1) `shouldBe` []

  describe "removeSet" $ do
    it "removes element from the set if it exists" $ do
      lengthSet (removeSet 2 $ insertSet 2 $ singleton 1) `shouldBe` 1

    it "does not remove element from the set if it does not exist" $ do
      lengthSet (removeSet 1 $ removeSet 3 $ singleton 2) `shouldBe` 1
      (peek 2 $ removeSet 1 $ singleton 2) `shouldBe` []

  describe "insertTag" $ do
    it "adds tag to element if it exists" $ do
      lengthSet (insertTag "b" 1 $ insertTag "a" 1 $ singleton 1) `shouldBe` 1
      (peek 1 $ insertTag "a" 1 $ insertTag "a" 1 $ singleton 1) `shouldBe` ["a"]
      (peek 1 $ insertTag "c" 1 $ insertTag "a" 1 $ insertTag "b" 1 $ singleton 1) `shouldBe` ["a","b","c"]

    it "does not add tag to element if it does not exist" $ do
      (peek 1 $ insertTag "a" 2 $ singleton 1) `shouldBe` []
      (peek 2 $ insertTag "a" 2 $ singleton 1) `shouldBe` []
      (peek 2 $ insertTag "a" 1 $ singleton 2) `shouldBe` []
      lengthSet (insertTag "a" 1 $ singleton 2) `shouldBe` 1

  describe "merge" $ do
    it "combines two sets with no repeated elements nor tags" $ do
      (lengthSet $ merge (insertSet 5 $ insertSet 2 $ singleton 1) $ merge (insertSet 4 $ insertSet 3 $ singleton 1) (singleton 2)) `shouldBe` 5
      (lengthSet $ merge (singleton 1) (insertSet 2 $ singleton 1)) `shouldBe` 2
      (peek 1 $ merge (insertTag "d" 1 $ insertTag "b" 1 $ insertTag "a" 1 $ singleton 1) (insertTag "c" 1 $ insertTag "b" 1 $ singleton 1)) `shouldBe` ["a","b","c","d"]
      (peek 1 $ merge (singleton 1) (insertTag "a" 1 $ singleton 1)) `shouldBe` ["a"]