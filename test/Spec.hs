{-# LANGUAGE TemplateHaskell #-}

module Main where

import           TastyTH104Behavior
import           Test.Tasty
import           Test.Tasty.TH

main :: IO ()
main = $(defaultMainGenerator)

test_test_group :: [IO TestTree]
test_test_group = [testgroup_TastyTH104Behavior]
