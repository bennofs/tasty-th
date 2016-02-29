-----------------------------------------------------------------------------
--
-- Module      :  Test.Tasty.TH
-- Copyright   :  Oscar Finnsson, Benno Fünfstück
-- License     :  BSD3
--
-- Maintainer  :  Benno Fünfstück
-- Stability   :
-- Portability :
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides TemplateHaskell functions to automatically generate
-- tasty TestTrees from specially named functions. See the README of the package
-- for examples.
--
-- Important: due to to the GHC staging restriction, you must put any uses of these
-- functions at the end of the file, or you may get errors due to missing definitions.
module Test.Tasty.TH
  ( testGroupGenerator
  , defaultMainGenerator
  , testGroupGeneratorFor
  , defaultMainGeneratorFor
  , extractTestFunctions
  , locationModule
  ) where

import           Control.Applicative
import           Control.Monad            (join)
import           Data.List
import           Data.Maybe
import           Language.Haskell.TH

import           Prelude
import           Test.Tasty

import           Test.Tasty.TestVarieties


-- #TODO:0 @Haddock
data Args = Args {
  testVarieties :: TestVarietiesAssocList
}

-- #TODO:10 @Haddock
defaultArgs = Args {
  testVarieties = defaultTestVarieties
}

-- #TODO:20 @Haddock
-- | Convenience function that directly generates an `IO` action that may be used as the
-- main function. It's just a wrapper that applies 'defaultMain' to the 'TestTree' generated
-- by 'testGroupGenerator'.
--
-- Example usage:
--
-- @
-- -- properties, test cases, ....
--
-- main :: IO ()
-- main = $('defaultMainGenerator')
-- @
defaultMainGenerator :: ExpQ
defaultMainGenerator = defaultMainGenerator' defaultArgs

-- #TODO:30 @Haddock
defaultMainGenerator' :: Args -> ExpQ
defaultMainGenerator' args = [| $(testGroupGenerator' args) >>= defaultMain  |]

-- | This function generates a 'TestTree' from functions in the current module.
-- The test tree is named after the current module.
--
-- The following definitions are collected by `testGroupGenerator`:
--
-- * a test_something definition in the current module creates a sub-testGroup with the name "something"
-- * a prop_something definition in the current module is added as a QuickCheck property named "something"
-- * a case_something definition leads to a HUnit-Assertion test with the name "something"
--
-- Example usage:
--
-- @
-- prop_example :: Int -> Int -> Bool
-- prop_example a b = a + b == b + a
--
-- tests :: 'TestTree'
-- tests = $('testGroupGenerator')
-- @
testGroupGenerator :: ExpQ
testGroupGenerator = testGroupGenerator' defaultArgs

testGroupGenerator' :: Args -> ExpQ
testGroupGenerator' args = join $ testGroupGeneratorFor' args <$> fmap loc_module location <*> testFunctions

 where
  testFunctions = location >>= runIO . extractTestFunctions . loc_filename

-- | Retrieves all function names from the given file that would be discovered by 'testGroupGenerator'.
extractTestFunctions :: FilePath -> IO [String]
extractTestFunctions = extractTestFunctions' defaultArgs

extractTestFunctions' :: Args -> FilePath -> IO [String]
extractTestFunctions' args filePath = do
  file <- readFile filePath
  let functions = map fst . concatMap lex . lines $ file
  return . nub $ mapMaybe (\t -> getVarietyForTest (testVarieties args) t >> return t) functions

-- | Extract the name of the current module.
locationModule :: ExpQ
locationModule = do
  loc <- location
  return $ LitE $ StringL $ loc_module loc

-- | Like 'testGroupGenerator', but generates a test group only including the specified function names.
-- The function names still need to follow the pattern of starting with one of @prop_@, @case_@ or @test_@.
testGroupGeneratorFor
  :: String   -- ^ The name of the test group itself
  -> [String] -- ^ The names of the functions which should be included in the test group
  -> ExpQ
testGroupGeneratorFor = testGroupGeneratorFor' defaultArgs

testGroupGeneratorFor'
  :: Args
  -> String   -- ^ The name of the test group itself
  -> [String] -- ^ The names of the functions which should be included in the test group
  -> ExpQ
testGroupGeneratorFor' args name functionNames = [| toTestTree name $(listE (mapMaybe test functionNames)) |]
 where
  test fname = do
    v <- getVarietyForTest (testVarieties args) fname
    return $ join $ either fail return <$> v fname

-- | Like 'defaultMainGenerator', but only includes the specific function names in the test group.
-- The function names still need to follow the pattern of starting with one of @prop_@, @case_@ or @test_@.
defaultMainGeneratorFor
  :: String   -- ^ The name of the top-level test group
  -> [String] -- ^ The names of the functions which should be included in the test group
  -> ExpQ
defaultMainGeneratorFor = defaultMainGeneratorFor' defaultArgs

defaultMainGeneratorFor'
  :: Args
  -> String   -- ^ The name of the top-level test group
  -> [String] -- ^ The names of the functions which should be included in the test group
  -> ExpQ
defaultMainGeneratorFor' args name fns = [| $(testGroupGeneratorFor' args name fns) >>= defaultMain |]
