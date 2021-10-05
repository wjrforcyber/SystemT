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

-- QuickCheck instance
gExp :: Gen Exp
gExp = oneof [pure Zero, Succ <$> gExp]

instance Arbitrary Exp where
  arbitrary = gExp

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps =
  testGroup
    "(Checked by QucikCheck)"
    [ QC.testProperty "Evaluate Zero" $
        (eval (Zero :: Exp)) == 0,
      QC.testProperty "Succ" $
        \x -> (eval (x :: Exp)) + 1 == ((eval (Succ (x))) :: Int),
      -- the following property does not hold
      QC.testProperty "Addition" $
        \x y ->
          (eval (x :: Exp)) + (eval (y :: Exp)) == (eval (Add x y) :: Int)
    ]

unitTests =
  testGroup
    "Unit tests"
    [ testCase "Zero" $
        eval (Zero) @?= 0,
      testCase "Succ" $
        (eval (Succ (Succ (Succ (Zero))))) @?= ((1 + (eval (Succ (Succ (Zero))))) :: Int),
      testCase "Addition" $
        (eval (Succ (Succ (Succ (Zero))))) + (eval (Succ (Succ (Zero)))) @?= (eval (Succ (Succ (Succ (Succ (Succ (Zero)))))) :: Int)
    ]
