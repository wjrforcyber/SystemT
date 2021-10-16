{-# LANGUAGE ScopedTypeVariables #-}

-- | This is a test suite.
module Main where

import Test.Tasty

import Lang.L1.Tests
import Lang.L2.Tests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [propertyTests, unitTests, l2Props, unitL2Tests]

propertyTests :: TestTree
propertyTests = testGroup "Property tests" [evalProps, parserProps]
