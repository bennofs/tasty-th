{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Test.Tasty.TH
import Test.Tasty
import           TastyTH104Behavior


test_test_group :: [IO TestTree]
test_test_group = [testgroup_TastyTH104Behavior]

main :: IO ()
main = $(defaultMainGenerator)
