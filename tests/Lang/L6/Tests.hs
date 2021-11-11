{-# LANGUAGE ScopedTypeVariables #-}

module Lang.L6.Tests (unitTests, propertyTests) where

import Data.Maybe
import Lang.L6.Eval.EEval as EE
-- import Lang.L5.Eval.IEval as I
import Lang.L6.Syntax.Extrinsic as E
-- import Lang.L5.Syntax.Intrinsic as I
import Lang.L6.Typecheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

propertyTests :: TestTree
propertyTests = testGroup "L6 Property tests" [tcL5Props, evalL5Props]

tcL5Props :: TestTree
tcL5Props =
  testGroup
    "Bidi-typecheck"
    [ QC.testProperty "every well-typed expression can be inferred" $
        \(e :: TcTyExp) ->
          tcisSuccess (tcinfer (tcgetExp e)),
      QC.testProperty "every well-typed expression can be checked for its type" $
        \(e :: TcTyExp) ->
          case runTC (tcinfer (tcgetExp e)) E.Emp of
            Right ty -> tcisSuccess $ tccheck (tcgetExp e) ty
            Left _ -> error $ "This cannot happen because" ++ show e ++ " is well-typed"
    ]

evalL5Props :: TestTree
evalL5Props =
  testGroup
    "eval"
    [ QC.testProperty "1-Well-typed expressions reduced to a value" $
        \(e :: TcTyExp) ->
          isJust $ EE.runEval (EE.eval (tcgetExp e)) EE.Emp
    ]

unitTests :: TestTree
unitTests = testGroup "L5 Unit tests" [unitL5Tests]

unitL5Tests :: TestTree
unitL5Tests =
  testGroup
    "L5"
    [ testCase "Unit on L6 check on Lambda" $
        tcisSuccess (tccheck (ELam (Name "x") TBool (EIf (EVar (Name "x")) (ESucc EZero) EZero)) (TFun TBool TNat)) @?= True,
      testCase "Unit on L6 infer on Lambda" $
        runTC (tcinfer (ELam (Name "x") TBool (EIf (EVar (Name "x")) (ESucc EZero) EZero))) E.Emp @?= Right (TFun TBool TNat),
      testCase "Unit on L6 1" $
        EE.runEval (EE.eval (EApp (ELam (Name "x") TBool (EIf (EVar (Name "x")) (ESucc EZero) EZero)) ETrue)) EE.Emp @?= Just (VSuccN 1)
    ]
