-- | This is a test suite.
module Main where

import Test.Tasty

import qualified Lang.L1.Tests as L1
import qualified Lang.L2.Tests as L2

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [L1.unitTests, L2.unitTests]

propertyTests :: TestTree
propertyTests = testGroup "Property tests" [L1.propertyTests, L2.propertyTests]
