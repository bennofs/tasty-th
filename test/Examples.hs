{-# LANGUAGE TemplateHaskell #-}
module Examples where

import           Control.Exception
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

prop_length_append :: [Int] -> [Int] -> Bool
prop_length_append as bs = length (as ++ bs) == length as + length bs

case_length_1 :: Assertion
case_length_1 = 1 @=? length [()]

case_add :: Assertion
case_add = (7 :: Integer) @=? (3 + 4)

test_plus :: [TestTree]
test_plus =
  [ testCase "3 + 4" $ (7 :: Integer) @=? (3 + 4)
    -- ...
  ]

spec_head :: Spec
spec_head = do
    describe "Prelude.head" $ do
      it "returns the first element of a list" $ do
        head [23 ..] `shouldBe` (23 :: Int)

      it "returns the first element of an *arbitrary* list" $
        property $ \x xs -> head (x:xs) == (x :: Int)

      it "throws an exception if used with an empty list" $ do
        evaluate (head []) `shouldThrow` anyException
