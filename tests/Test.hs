{-# LANGUAGE ScopedTypeVariables #-}

-- | This is a test suite.
module Main where

import Common.Types
import Lang.L1.Eval
import Lang.L1.Syntax
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [propertyTests, unitTests]

propertyTests :: TestTree
propertyTests = testGroup "Property tests" [qcProps]

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck tests"
    [ QC.testProperty "Nat" $
        \(x :: Nat) -> eval (ENat x) == x,
      QC.testProperty "Addition" $
        \(x :: Exp) (y :: Exp) ->
          eval x + eval y == eval (EAdd x y),
      QC.testProperty "Multiplication" $
        \(x :: Exp) (y :: Exp) ->
          eval x * eval y == eval (EMul x y)
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "Zero" $
        eval (ENat Zero) @?= 0,
      testCase "Nat" $
        eval (ENat 3) @?= 3,
      testCase "Addition" $
        eval (EAdd (EMul (ENat 2) (ENat 3)) (ENat 3)) @?= 9,
      testCase "Multiplication" $
        eval (EMul (ENat 2) (ENat 3)) @?= 6
    ]
