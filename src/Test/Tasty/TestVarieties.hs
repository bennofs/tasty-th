{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Tasty.TestVarieties where

import           Control.Applicative
import           Data.List
import           Data.Maybe
import           Language.Haskell.TH
import           Test.Tasty

data TestVariety = TestVariety {
    shouldIncludeTest :: String -> Maybe String -- | given a top-level definition, returns `Nothing` if the definition should be excluded and `Just name` to include a test and name it
  , makeTestE         :: String -> Q Exp -- | give a test, returns  a a splice that generates a function of Type `a -> IO TestTree`. This function will be applied to top-level definitions that are accepted by shouldIncludeTest `
}

return' :: a -> IO a
return' = return

fixName :: String -> String
fixName = replace '_' ' ' . tail . dropWhile (/= '_')

replace :: Eq a => a -> a -> [a] -> [a]
replace b v = map (\i -> if b == i then v else i)

shouldIncludeByPrefix :: String -> String -> Maybe String
shouldIncludeByPrefix p t = if p `isPrefixOf` t then Just $ fixName t else Nothing

makeTest :: String -> String -> Q Exp
makeTest functionName testName = do
  mv <- lookupValueName functionName
  v <- maybe (fail $ show functionName ++ " not found") (return . VarE) mv
  return $ AppE v (LitE $ StringL testName)

class TastyGroupable a where
  toTestTree :: String -> a -> IO TestTree

instance TastyGroupable TestTree where
  toTestTree _ = return

instance TastyGroupable [TestTree] where
  toTestTree n ts = return $ testGroup n ts

instance TastyGroupable (IO TestTree) where
  toTestTree _ = id


instance TastyGroupable (IO [TestTree]) where
  toTestTree n iots = testGroup n <$> iots

instance TastyGroupable [IO TestTree] where
  toTestTree n = toTestTree n . sequenceA
-- |Matches top-level definitions that have a prefix "Mase_"
-- |Creates a TestTree with Test.Tasty.HUnit.testCase

hunitVariety :: TestVariety
hunitVariety = TestVariety {
    shouldIncludeTest = shouldIncludeByPrefix "case_"
  , makeTestE = \n ->  [| return' . $(makeTest "testCase" n)|]
}

quickCheckVariety :: TestVariety
quickCheckVariety = TestVariety {
    shouldIncludeTest = (<|>) <$> shouldIncludeByPrefix "prop_" <*> shouldIncludeByPrefix "qcprop_"
  , makeTestE = \n ->  [| return' . $(makeTest "Test.Tasty.QuickCheck.testProperty" n) |]
}

testVariety :: TestVariety
testVariety = TestVariety {
    shouldIncludeTest = shouldIncludeByPrefix "test_"
  , makeTestE = \n ->  [| toTestTree n|]
}

specVariety :: TestVariety
specVariety = TestVariety {
    shouldIncludeTest = shouldIncludeByPrefix "spec_"
  , makeTestE = \n ->  [| $(makeTest "testSpec" n) |]
}

defaultTestVarieties :: [TestVariety]
defaultTestVarieties = [hunitVariety, quickCheckVariety, testVariety, specVariety]

getVarietyForTest :: [TestVariety] -> String -> Maybe TestVariety
getVarietyForTest vs t = listToMaybe . catMaybes $ varietyIfItMatches <$> vs
  where
    varietyIfItMatches v = shouldIncludeTest v t >> return v
