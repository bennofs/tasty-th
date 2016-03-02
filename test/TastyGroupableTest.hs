{-# LANGUAGE TemplateHaskell #-}
module TastyGroupableTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import           Examples

testgroup_tasty_groupable :: IO TestTree
testgroup_tasty_groupable = $(testGroupGeneratorIO)

test_tasty_groupable_instance_TestTree :: TestTree
test_tasty_groupable_instance_TestTree = testProperty "test_tasty_groupable_instance_TestTree_prop" prop_length_append

test_tasty_groupable_instance_list_of_TestTree :: [TestTree]
test_tasty_groupable_instance_list_of_TestTree = [
      testProperty "test_tasty_groupable_instance_T_list_of_TestTree_prop" prop_length_append
    , testGroup "test_tasty_groupable_instance_T_list_of_TestTree_prop" test_plus
  ]

test_tasty_groupable_instance_T_list_of_IO_TestTree :: [IO TestTree]
test_tasty_groupable_instance_T_list_of_IO_TestTree = return <$> test_tasty_groupable_instance_list_of_TestTree

test_tasty_groupable_instance_T_IO_TestTree :: IO [TestTree]
test_tasty_groupable_instance_T_IO_TestTree = sequenceA test_tasty_groupable_instance_T_list_of_IO_TestTree
