{-# LANGUAGE TemplateHaskell #-}
module DefaultMainForTest where

import           Examples
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

main :: IO ()
main = $(defaultMainGeneratorFor "explicit" ["prop_length_append", "case_length_1", "test_plus", "spec_head"])
