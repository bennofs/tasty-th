{-# LANGUAGE TemplateHaskell #-}
module TestGroupGeneratorTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import Examples             

testgroup_test_group_generator :: TestTree
testgroup_test_group_generator = $(testGroupGenerator)

{-
prop_length_append
case_length_1
test_plus
-}
