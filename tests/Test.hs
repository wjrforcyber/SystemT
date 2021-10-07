-- | This is a test suite.
module Main where

import Common.Types
import Lang.L1.Eval
import Lang.L1.Syntax
import Text.Parsec

import Test.QuickCheck.Gen
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

instance Arbitrary Exp where
  arbitrary = sized arbExp

arbExp :: Int -> Gen Exp
arbExp n = oneof [ENat <$> arbitrary]


tests :: TestTree
tests = testGroup "Tests" [propertyTests, unitTests]

propertyTests :: TestTree
propertyTests = testGroup "Property tests" [qcProps]

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck tests"
    [ QC.testProperty "ENat->Nat" $
        \x -> eval ((ENat x) :: Exp)  == (x :: Nat),
      QC.testProperty "Addition" $
        \x y ->
          eval (x :: Exp) + eval (y :: Exp) == (eval (EAdd x y) :: Nat),
      QC.testProperty "Multiplication" $
        \x y ->
          eval (x :: Exp) * eval (y :: Exp) == (eval (EMul x y) :: Nat)]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "Zero" $
        eval (ENat Zero) @?= 0,
      testCase "Exp -> Nat" $
        eval (ENat 3) @?= 3,
      testCase "Addition" $
        eval (EAdd (ENat 2) (ENat 3)) @?= 5,
      testCase "Nultiplucation"$
        eval (EMul (ENat 2) (ENat 3)) @?= 6]