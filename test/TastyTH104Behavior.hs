{-# LANGUAGE TemplateHaskell #-}
module TastyTH104Behavior where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH
prop_length_append :: [Int] -> [Int] -> Bool
prop_length_append as bs = length (as ++ bs) == length as + length bs

case_length_1 :: Assertion
case_length_1 = 1 @=? length [()]

test_plus :: [TestTree]
test_plus =
  [ testCase "3 + 4" ((7 :: Integer) @=? (3 + 4))
    -- ...
  ]

testgroup_TastyTH104Behavior :: IO TestTree
testgroup_TastyTH104Behavior = $(testGroupGenerator)
