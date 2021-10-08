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
arbExp 0 = oneof [pure (ENat Zero), ENat <$> arbitrary]
arbExp n | n > 0 = do
  (Positive m) <- arbitrary
  let subExp = arbExp (n `div` (m + 1))
  oneof [subExp, EAdd <$> subExp <*> subExp, EMul <$> subExp <*> subExp]


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
          eval (x :: Exp) * eval (y :: Exp) == (eval (EMul x y) :: Nat) ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "Zero" $
        eval (ENat Zero) @?= 0,
      testCase "Exp -> Nat" $
        eval (ENat 3) @?= 3,
      testCase "Addition" $
        eval (EAdd (EMul (ENat 2) (ENat 3)) (ENat 3)) @?= 9,
      testCase "Nultiplucation"$
        eval (EMul (ENat 2) (ENat 3)) @?= 6]