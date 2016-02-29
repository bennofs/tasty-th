{-# LANGUAGE TemplateHaskell #-}
module SpecVarieties where

import           Control.Exception.Base
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

testgroup_SpecVarieties :: IO TestTree
testgroup_SpecVarieties = $(testGroupGenerator)

prop_length_append :: [Int] -> [Int] -> Bool
prop_length_append as bs = length (as ++ bs) == length as + length bs

case_length_1 :: Assertion
case_length_1 = 1 @=? length [()]

test_plus :: [TestTree]
test_plus =
  [ testCase "3 + 4" ((7 :: Integer) @=? (3 + 4))
    -- ...
  ]

spec_example_head :: Spec
spec_example_head = do
    describe "Prelude.head" $ do
      it "returns the first element of a list" $ do
        head [23 ..] `shouldBe` (23 :: Int)

      it "returns the first element of an *arbitrary* list" $
        property $ \x xs -> head (x:xs) == (x :: Int)

      it "throws an exception if used with an empty list" $ do
        evaluate (head []) `shouldThrow` anyException

test_TastyGroupableInstance_TestTree :: TestTree
test_TastyGroupableInstance_TestTree = testProperty "test_TastyGroupableInstanceTestTree_prop" prop_length_append

test_TastyGroupableInstance_list_of_TestTree :: [TestTree]
test_TastyGroupableInstance_list_of_TestTree = [
      testProperty "test_TastyGroupableInstanceTestTree_prop" prop_length_append
    , testGroup "test_TastyGroupableInstanceTestTree_group" test_plus
  ]

test_TastyGroupableInstance_list_of_IO_TestTree :: [IO TestTree]
test_TastyGroupableInstance_list_of_IO_TestTree = return <$> test_TastyGroupableInstance_list_of_TestTree

test_TastyGroupableInstance_IO_TestTree :: IO [TestTree]
test_TastyGroupableInstance_IO_TestTree = sequenceA test_TastyGroupableInstance_list_of_IO_TestTree
