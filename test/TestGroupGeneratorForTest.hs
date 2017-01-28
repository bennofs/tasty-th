{-# LANGUAGE TemplateHaskell #-}
module TestGroupGeneratorForTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import           Examples

testgroup_test_group_generator_for :: TestTree
testgroup_test_group_generator_for = $(testGroupGeneratorFor $locationModule ["prop_length_append", "case_length_1", "test_plus"])
