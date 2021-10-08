{-# LANGUAGE ScopedTypeVariables #-}

-- | This is a test suite.
module Main where

import Common.Types
import Lang.L1.Eval
import Lang.L1.Syntax
import qualified Prettyprinter as PP
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified Text.Parsec as P

main :: IO ()
main = defaultMain tests

parse :: String -> Either P.ParseError Exp
parse = P.parse parseExp ""

pp :: Exp -> String
pp = show . PP.pretty

tests :: TestTree
tests = testGroup "Tests" [propertyTests, unitTests]

propertyTests :: TestTree
propertyTests = testGroup "Property tests" [evalProps, parserProps]

evalProps :: TestTree
evalProps =
  testGroup
    "eval"
    [ QC.testProperty "eval of Nat is itself" $
        \(x :: Nat) -> eval (ENat x) == x,
      QC.testProperty "eval of Add is +" $
        \(x :: Exp) (y :: Exp) ->
          eval x + eval y == eval (EAdd x y),
      QC.testProperty "eval of Mul is *" $
        \(x :: Exp) (y :: Exp) ->
          eval x * eval y == eval (EMul x y)
    ]

parserProps :: TestTree
parserProps =
  testGroup
    "parse/prettyprint"
    [ QC.testProperty "parse . pp is identity" $
        \(e :: Exp) -> (parse . pp) e == Right e
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [evalUnitTests, parserUnitTests]

evalUnitTests :: TestTree
evalUnitTests =
  testGroup
    "eval"
    [ testCase "Zero" $
        eval (ENat Zero) @?= 0,
      testCase "Nat" $
        eval (ENat 3) @?= 3,
      testCase "Addition" $
        eval (EAdd (EMul (ENat 2) (ENat 3)) (ENat 3)) @?= 9,
      testCase "Multiplication" $
        eval (EMul (ENat 2) (ENat 3)) @?= 6
    ]

parserUnitTests :: TestTree
parserUnitTests =
  testGroup
    "parser"
    [ testCase "2 + 3 * 4" $
        parse "2 + 3 * 4" @?= Right (EAdd (ENat 2) (EMul (ENat 3) (ENat 4)))
    ]
