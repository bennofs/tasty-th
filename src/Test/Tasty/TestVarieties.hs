{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tasty.TestVarieties where

import           Control.Applicative
import           Control.Monad
import           Data.Either
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Language.Haskell.TH
import           Test.Tasty


type TestVariety = String -> Q (Either String Exp)

type TestVarietiesAssocList = [(String, TestVariety)]
--data TestVariety = TestVariety {
--    shouldIncludeTest :: String -> Maybe String -- | given a top-level definition, returns `Nothing` if the definition should be excluded and `Just name` to include a test and name it
--  , makeTestE         :: String -> Q Exp -- | give a test, returns  a a splice that generates a function of Type `a -> IO TestTree`. This function will be applied to top-level definitions that are accepted by shouldIncludeTest `
--}

--return' :: a -> IO a
--return' = return

fixName :: String -> String
fixName = replace '_' ' ' . tail . dropWhile (/= '_')

replace :: Eq a => a -> a -> [a] -> [a]
replace b v = map (\i -> if b == i then v else i)

--shouldIncludeByPrefix :: String -> String -> Maybe String
--shouldIncludeByPrefix p t = if p `isPrefixOf` t then Just $ fixName t else Nothing

makeTestSplice :: [String] -> TestVariety
makeTestSplice makeTestFunctions fname = do
  maybeCreateTestName <- getFirst . foldMap First <$> traverse lookupValueName makeTestFunctions
  maybeTestFunName <- lookupValueName fname
  let makeTestFunctionsError = "None of the following functions are in scope: " ++ show makeTestFunctions
      testFunError = "Function for test \"" ++ fname ++ "\" not in scope"
      createTestName = maybe (Left makeTestFunctionsError) (Right . VarE) maybeCreateTestName
      testFunName = maybe (Left testFunError) (Right . VarE) maybeTestFunName
      (errors, ~(ct : tf : _)) = partitionEithers [createTestName, testFunName]
  if null errors
    then return <$> [| toTestTree "" $ $(return ct) (fixName fname) $(return tf) |]
    else return . Left . unlines $ errors

class TastyGroupable a where
  toTestTree :: String -> a -> IO TestTree

instance TastyGroupable TestTree where
  toTestTree _ = return


instance Foldable f => TastyGroupable (f TestTree) where
  toTestTree n ts = return $ testGroup n $ toList ts

instance TastyGroupable (IO TestTree) where
  toTestTree _ = id


instance Foldable f => TastyGroupable (IO (f TestTree)) where
  toTestTree n iots = testGroup n . toList <$> iots

instance Traversable t =>  TastyGroupable (t (IO TestTree)) where
  toTestTree n = toTestTree n . sequenceA
-- |Matches top-level definitions that have a prefix "Mase_"
-- |Creates a TestTree with Test.Tasty.HUnit.testCase

hunitVariety :: TestVariety
hunitVariety = makeTestSplice [
    "Test.Tasty.Hunit.testCase"
  , "Hunit.testCase"
  , "hunitTestCase"
  , "THU.testCase"
  , "HU.testCase"
  , "testCase"
  ]
  {-TestVariety {
    shouldIncludeTest = shouldIncludeByPrefix "case_"
  , makeTestE = \n ->  [| return' . $(makeTest "testCase" n)|]
}-}

quickCheckVariety :: TestVariety
quickCheckVariety = makeTestSplice [
    "Test.Tasty.QuickCheck.testProperty"
  , "TQC.testProperty"
  , "quickCheckTestProperty"
  , "QC.testProperty"
  , "testProperty"
  ]

{-TestVariety {
    shouldIncludeTest = (<|>) <$> shouldIncludeByPrefix "prop_" <*> shouldIncludeByPrefix "qcprop_"
  , makeTestE = \n ->  [| return' . $(makeTest "Test.Tasty.QuickCheck.testProperty" n) |]
}-}

testVariety :: TestVariety
testVariety fname =  do
  testFunName <- lookupValueName fname
  maybe (return . Left $ "Function for test \"" ++ fname ++ "\" not in scope")
        (\n -> Right <$> [|toTestTree (fixName fname) $(return $ VarE n)|])
        testFunName
{-TestVariety {
    shouldIncludeTest = shouldIncludeByPrefix "test_"
  , makeTestE = \n ->  [| toTestTree n|]
}-}

specVariety :: TestVariety
specVariety = makeTestSplice [
    "Test.Tasty.HSpec.testSpec"
  , "THS.testSpec"
  , "HS.testSpec"
  , "hspecTestSpec"
  , "testSpec"
  ]
{-
  TestVariety {
    shouldIncludeTest = shouldIncludeByPrefix "spec_"
  , makeTestE = \n ->  [| $(makeTest "testSpec" n) |]
}
-}

defaultTestVarieties :: [(String, TestVariety)]
defaultTestVarieties = [
    ("case_", hunitVariety)
  , ("qcprop_", quickCheckVariety)
  , ("prop_", quickCheckVariety)
  , ("test_", testVariety)
  , ("spec_", specVariety)
  ]

getVarietyForTest :: TestVarietiesAssocList -> String -> Maybe TestVariety
getVarietyForTest vs fname = fmap snd . listToMaybe . filter ((`isPrefixOf` fname) . fst) $ vs
