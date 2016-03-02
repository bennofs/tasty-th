{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified DefaultMainForTest          as DM1
import qualified DefaultMainTest             as DM2
import           TastyGroupableTest
import           Test.Tasty
import           Test.Tasty.TestVarieties
import           Test.Tasty.TH
import           TestGroupGeneratorForIOTest
import           TestGroupGeneratorForTest
import           TestGroupGeneratorIOTest
import           TestGroupGeneratorTest

main :: IO ()
main = $(defaultMainGenerator)
  >> DM1.main
  >> DM2.main

test_main_test_group :: [IO TestTree]
test_main_test_group= [
    testgroup_tasty_groupable
  , testgroup_test_group_generator_for_io
  , toTestTree "" testgroup_test_group_generator_for
  , testgroup_test_group_generator_io
  , toTestTree "" testgroup_test_group_generator
  ]
