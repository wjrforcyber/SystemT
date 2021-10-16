{-# LANGUAGE ScopedTypeVariables #-}

-- | This is a test suite.
module Main where

import Common.Types
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC


import L1tests
import L2tests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [propertyTests, unitTests, opUnitL2Tests]

propertyTests :: TestTree
propertyTests = testGroup "Property tests" [evalProps, parserProps]
