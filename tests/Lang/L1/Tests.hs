{-# LANGUAGE ScopedTypeVariables #-}
module Lang.L1.Tests (unitTests, propertyTests) where

import Common.Types
import Lang.L1.Eval
import Lang.L1.Syntax

import qualified Prettyprinter as PP
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified Text.Parsec as P

parse :: String -> Either P.ParseError Exp
parse = P.parse parseExp ""

pp :: Exp -> String
pp = show . PP.pretty

propertyTests :: TestTree
propertyTests = testGroup "L1 Property tests" [evalUnitTests, parserUnitTests, opUnitL1Tests]

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
          eval x * eval y == eval (EMul x y),
      QC.testProperty "opt has no Muls" $
        \(e :: Exp) -> not $ hasEMul (opt e),
      QC.testProperty "opt doesn't change eval" $
        \(e :: Exp) -> eval e == eval (opt e),
      QC.testProperty "opt2 has no Muls" $
        \(e :: Exp) -> not $ hasEMul (opt2 e),
      QC.testProperty "opt2 doesn't change eval" $
        \(e :: Exp) -> eval e == eval (opt2 e)
    ]

parserProps :: TestTree
parserProps =
  testGroup
    "parse/prettyprint"
    [ QC.testProperty "parse . pp is identity" $
        \(e :: Exp) -> (parse . pp) e == Right e
    ]

unitTests :: TestTree
unitTests = testGroup "L1 Unit tests" [evalUnitTests, parserUnitTests, opUnitL1Tests]

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

opUnitL1Tests :: TestTree
opUnitL1Tests =
  testGroup
    "opt"
    [ testCase "ENat" $
        eval (opt (unsafeParse "2")) @?= eval (unsafeParse "2"),
      testCase "EAdd" $
        eval (opt (unsafeParse "2+3")) @?= eval (unsafeParse "2+3"),
      testCase "Simple Mul" $
        eval (opt (unsafeParse "2*3")) @?= eval (unsafeParse "2*3"),
      testCase "Addition test 0" $
        eval (opt (unsafeParse "3*2+4")) @?= eval (unsafeParse "3*2+4"),
      testCase "Addition test 1" $
        eval (opt (unsafeParse "1+2*6")) @?= eval (unsafeParse "1+2*6"),
      testCase "Addition test 2" $
        eval (opt (unsafeParse "1+2+6")) @?= eval (unsafeParse "1+2+6"),
      testCase "Addition test 3" $
        eval (opt (unsafeParse "(2*3)+(3*4)")) @?= eval (unsafeParse "(2*3)+(3*4)"),
      testCase "Addition test 4" $
        eval (opt (unsafeParse "2*3+3*4")) @?= eval (unsafeParse "2*3+3*4"),
      testCase "Addition test 5" $
        eval (opt (unsafeParse "1*0*0")) @?= eval (unsafeParse "1*0*0"),
      testCase "Addition test 6" $
        eval (opt (unsafeParse "(6*13)*(2*9)")) @?= eval (unsafeParse "(6*13)*(2*9)"),
      testCase "Addition test 7" $
        opt (unsafeParse "2+3*4") @?= unsafeParse "2+3+3+3+3+0",
      --  testCase "Addition test 8"$
      --   opt (unsafeParse "2+3*4") @?= unsafeParse "2+4+4+4+0",
      --  testCase "Addition test 9"$
      --   opt2 (unsafeParse "2+3*4") @?= unsafeParse "2+3+3+3+3+0",
      testCase "Addition test 10" $
        opt2 (unsafeParse "2+3*4") @?= unsafeParse "2+4+4+4+0"
    ]
