{-# LANGUAGE ScopedTypeVariables #-}

-- | This is a test suite.
module Main where

import Common.Types
-- import Lang.L1.Eval
-- import Lang.L1.Syntax

-- import qualified Prettyprinter as PP
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
-- import qualified Text.Parsec as P

import L1tests
import L2tests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [propertyTests, unitTests, opUnitL2Tests]

propertyTests :: TestTree
propertyTests = testGroup "Property tests" [evalProps, parserProps]
