-- | This is a test suite.
module Main where

import Lang.L1.Eval
import Lang.L1.Syntax
import Test.QuickCheck.Gen
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

instance Arbitrary Exp where
  arbitrary = sized arbExp

arbExp :: Int -> Gen Exp
arbExp 0 = oneof [pure Zero, Succ <$> arbitrary]
arbExp n | n > 0 = do
  (Positive m) <- arbitrary
  let subExp = arbExp (n `div` (m + 1))
  oneof [pure Zero, Succ <$> subExp, Add <$> subExp <*> subExp, Mul <$> subExp <*> subExp]

tests :: TestTree
tests = testGroup "Tests" [propertyTests, unitTests]

propertyTests :: TestTree
propertyTests = testGroup "Property tests" [qcProps]

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck tests"
    [ QC.testProperty "Zero" $
        eval (Zero :: Exp) == 0,
      QC.testProperty "Succ" $
        \x -> eval (x :: Exp) + 1 == (eval (Succ x) :: Int),
      QC.testProperty "Addition" $
        \x y ->
          eval (x :: Exp) + eval (y :: Exp) == (eval (Add x y) :: Int),
      QC.testProperty "Multiplication" $
        \x y ->
          eval (x :: Exp) * eval (y :: Exp) == (eval (Mul x y) :: Int)
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "Zero" $
        eval Zero @?= 0,
      testCase "Succ" $
        eval (Succ (Succ (Succ Zero))) @?= ((1 + eval (Succ (Succ Zero))) :: Int),
      testCase "Addition" $
        eval (Succ (Succ (Succ Zero))) + eval (Succ (Succ Zero)) @?= (eval (Succ (Succ (Succ (Succ (Succ Zero))))) :: Int)
    ]
