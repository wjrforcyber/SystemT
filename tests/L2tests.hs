module L2tests where

import Lang.L2.Typecheck
import Lang.L2.Syntax

import Test.Tasty
import Test.Tasty.HUnit


opUnitL2Tests :: TestTree
opUnitL2Tests =
  testGroup
    "L2"
    [
      testCase "Unit on L2 0" $
        check (EIf (EBool True) (ENat 1) (ENat 3)) Nat @?= True,
      testCase "Unit on L2 1" $
        check (EIf (EBool False) (ENat 2) (EAdd (ENat 1) (ENat 3))) Nat @?= True,
      testCase "Unit on L2 2" $
        check (EIf (EBool (((False && False) || (True)))) (EBool True) (EBool False)) Bool @?= True,
      testCase "Unit on L2 3" $
        check (EAdd (ENat 1) (ENat 2)) Nat @?= True,
      testCase "Unit on L2 4" $
        check (EAdd (EAdd (ENat 1) (ENat 2)) (EAdd (ENat 3) (ENat 4))) Nat @?= True,
      testCase "Unit on L2 5" $
        check (EAdd (ENat 1) (ENat 2)) Nat @?= True,
      testCase "Unit on L2 6" $
        check (ENat 1) Bool @?= False,
      testCase "Unit on L2 7" $
        check (EIf (ENat 3) (ENat 1) (ENat 2)) Nat @?= False

    ]
