-- | This is a test suite.
module Main where

import qualified Lang.L1.Tests as L1
import qualified Lang.L2.Tests as L2
import qualified Lang.L3.Tests as L3
import qualified Lang.L4.Tests as L4
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [L1.unitTests, L2.unitTests]

propertyTests :: TestTree
propertyTests = testGroup "Property tests" [L1.propertyTests, L2.propertyTests, L3.propertyTests, L4.propertyTests]
