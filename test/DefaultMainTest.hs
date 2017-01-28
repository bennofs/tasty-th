{-# LANGUAGE TemplateHaskell #-}
module DefaultMainTest where

import           Examples
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

{-
prop_length_append
case_length_1
test_plus
spec_head
-}

main :: IO ()
main = $(defaultMainGenerator)
