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


data TestVarietySpliceError =
    MakeTestFunctionNotInScope [String]
  | TestFunctionNotInScope String
  | TestFunctionRequiresIO String
  deriving (Eq, Show)

data TestTreeSpliceMode = TestTreeMode | IOTestTreeMode deriving(Eq, Show)

type TestVariety = String -> TestTreeSpliceMode -> Q (Either [TestVarietySpliceError] Exp)
type TestVarietiesAssocList = [(String, TestVariety)]

fixName :: String -> String
fixName = replace '_' ' ' . tail . dropWhile (/= '_')

replace :: Eq a => a -> a -> [a] -> [a]
replace b v = map (\i -> if b == i then v else i)

--shouldIncludeByPrefix :: String -> String -> Maybe String
--shouldIncludeByPrefix p t = if p `isPrefixOf` t then Just $ fixName t else Nothing

testVarietyFromMakeTestFunctions :: [String] -> TestVariety
testVarietyFromMakeTestFunctions makeTestFunctions fname mode = do
  maybeCreateTestName <- getFirst . foldMap First <$> traverse lookupValueName makeTestFunctions
  maybeTestFunName <- lookupValueName fname
  let createTestName = maybe (Left $ MakeTestFunctionNotInScope makeTestFunctions)
                             (Right . VarE)
                             maybeCreateTestName
      testFunName = maybe (Left $ TestFunctionNotInScope fname)
                          (Right . VarE)
                          maybeTestFunName
      (errors, ~(ct : tf : _)) = partitionEithers [createTestName, testFunName]
      resultSplice = [| $(return ct) (fixName fname) $(return tf) |]
      resultSplice' = if mode == IOTestTreeMode then [|toTestTree "" $resultSplice |] else resultSplice
  if null errors
    then return <$> resultSplice'
    else return $ Left errors

mustBeInIO :: TestVariety -> TestVariety
mustBeInIO v fname mode =
  if mode /= IOTestTreeMode
  then return $ Left [TestFunctionRequiresIO fname]
  else v fname mode

class TastyGroupable a where
  toTestTree :: String -> a -> IO TestTree

instance TastyGroupable TestTree where
  toTestTree _ = return

instance TastyGroupable a => TastyGroupable (IO a) where
    toTestTree n = (>>= toTestTree n)

instance TastyGroupable a => TastyGroupable [a] where
  toTestTree n ts = testGroup n <$> traverse (toTestTree "") ts

hunitVariety :: TestVariety
hunitVariety = testVarietyFromMakeTestFunctions [
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
quickCheckVariety = testVarietyFromMakeTestFunctions [
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
testVariety fname mode =  do
  testFunName <- lookupValueName fname
  let makeTestFunction =
        case mode of
          TestTreeMode -> [|testGroup|]
          IOTestTreeMode -> [|toTestTree|]
  maybe (return $ Left [TestFunctionNotInScope fname])
        (\n -> Right <$> [|$makeTestFunction (fixName fname) $(return $ VarE n)|])
        testFunName
{-TestVariety {
    shouldIncludeTest = shouldIncludeByPrefix "test_"
  , makeTestE = \n ->  [| toTestTree n|]
}-}

specVariety :: TestVariety
specVariety = mustBeInIO $ testVarietyFromMakeTestFunctions [
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
