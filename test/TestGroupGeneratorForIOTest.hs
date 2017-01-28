{-# LANGUAGE TemplateHaskell #-}
module TestGroupGeneratorForIOTest where

import           Examples
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

testgroup_test_group_generator_for_io :: IO TestTree
testgroup_test_group_generator_for_io = $(testGroupGeneratorForIO $locationModule ["prop_length_append", "case_length_1", "test_plus", "spec_head"])
