{-# LANGUAGE ScopedTypeVariables #-}
module Lang.L2.Tests (unitTests, propertyTests) where

import Lang.L2.Typecheck
import Lang.L2.Syntax

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

propertyTests :: TestTree
propertyTests = testGroup "L2 Property tests" [l2Props]

l2Props :: TestTree
l2Props =
  testGroup
    "eval"
    [ QC.testProperty "check test EAdd" $
        \(e1 :: Exp) (e2 :: Exp) ->
          check (EAdd e1 e2) Nat == check e1 Nat && check e2 Nat,
      QC.testProperty "check test EMul" $
        \(e1 :: Exp) (e2 :: Exp) ->
          check (EMul e1 e2) Nat == check e1 Nat && check e2 Nat,
      QC.testProperty "check test EIf 0" $
        \(e1 :: Exp) (e2 :: Exp) (e3 :: Exp) ->
          check (EIf e1 e2 e3) Nat == check e1 Bool && (check e2 Nat || check e3 Nat),
      QC.testProperty "check test EIf 1" $
        \(e1 :: Exp) (e2 :: Exp) (e3 :: Exp)->
          check (EIf e1 e2 e3) Bool == check e1 Bool && (check e2 Bool || check e3 Bool)
    ]

unitTests :: TestTree
unitTests = testGroup "L2 Property tests" [unitL2Tests]

unitL2Tests :: TestTree
unitL2Tests =
  testGroup
    "L2"
    [
      testCase "Unit on L2 0" $
        check (EIf (EBool True) (ENat 1) (ENat 3)) Nat @?= True,
      testCase "Unit on L2 1" $
        check (EIf (EBool False) (ENat 2) (EAdd (ENat 1) (ENat 3))) Nat @?= True,
      testCase "Unit on L2 2" $
        check (EIf (EBool ((False && False) || (True))) (EBool True) (EBool False)) Bool @?= True,
      testCase "Unit on L2 3" $
        check (EAdd (ENat 1) (ENat 2)) Nat @?= True,
      testCase "Unit on L2 4" $
        check (EAdd (EAdd (ENat 1) (ENat 2)) (EAdd (ENat 3) (ENat 4))) Nat @?= True,
      testCase "Unit on L2 5" $
        check (EAdd (ENat 1) (ENat 2)) Nat @?= True,
      testCase "Unit on L2 6" $
        check (ENat 1) Bool @?= False,
      testCase "Unit on L2 7" $
        check (EIf (ENat 3) (ENat 1) (ENat 2)) Nat @?= False,
      testCase "Unit on L2 infer 0" $
        infer (ENat 1) @?= Just Nat,
      testCase "Unit on L2 infer 1" $
        infer (EBool True) @?= Just Bool,
      testCase "Unit on L2 infer 2" $
        infer (EAdd (ENat 1) (ENat 2)) @?= Just Nat,
      testCase "Unit on L2 infer 3" $
        infer (EBool (True && False)) @?= Just Bool,
      testCase "Unit on L2 infer 4" $
        infer (EIf (EBool True) (EAdd (ENat 1) (ENat 2)) (EMul (ENat 3)(ENat 4))) @?= Just Nat,
      testCase "Unit on L2 infer 5" $
        infer (EIf (EIf (EBool True) (EBool True) (EBool True)) (EAdd (ENat 1) (ENat 2)) (EMul (ENat 3)(ENat 4))) @?= Just Nat
    ]
