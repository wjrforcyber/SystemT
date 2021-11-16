{-# LANGUAGE ScopedTypeVariables #-}

module Lang.L6.Tests (unitTests, propertyTests) where

import Lang.L6.Eval.EEval as EE
-- import Lang.L5.Eval.IEval as I
import Lang.L6.Syntax.Extrinsic as E
-- import Lang.L5.Syntax.Intrinsic as I
import Lang.L6.Typecheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

propertyTests :: TestTree
propertyTests = testGroup "L6 Property tests" [tcL6Props, evalL6Props]

tcL6Props :: TestTree
tcL6Props =
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

evalL6Props :: TestTree
evalL6Props =
  testGroup
    "eval"
    [ QC.testProperty "1. Progress: Well-typed expressions always reduce to a value" $
        QC.within 10000000 $
          \(e :: TcTyExp) ->
            EE.isVal (EE.eval (tcgetExp e)),
      QC.testProperty "2. Type-preservation: Well-typed expressions reduce to a value of the same type" $
        QC.within 10000000 $
          \(e :: TcTyExp) ->
            case runTC (tcinfer (tcgetExp e)) E.Emp of
              Right ty -> tcisSuccess $ tccheck (EE.eval (tcgetExp e)) ty
              Left _ -> error $ "This cannot happen because" ++ show e ++ " is well-typed"
    ]

unitTests :: TestTree
unitTests = testGroup "L6 Unit tests" [unitL6Tests]

unitL6Tests :: TestTree
unitL6Tests =
  testGroup
    "L6"
    [ testCase "Unit on L6 check on Lambda" $
        tcisSuccess (tccheck (ELam (Name "x") TBool (EIf (EVar (Name "x")) (ESucc EZero) EZero)) (TFun TBool TNat)) @?= True,
      testCase "Unit on L6 infer on Lambda" $
        runTC (tcinfer (ELam (Name "x") TBool (EIf (EVar (Name "x")) (ESucc EZero) EZero))) E.Emp @?= Right (TFun TBool TNat)
        {- testCase "Unit on L6 1" $
          EE.runEval (EE.eval (EApp (ELam (Name "x") TBool (EIf (EVar (Name "x")) (ESucc EZero) EZero)) ETrue)) EE.Emp @?= Just (VSuccN 1),
        testCase "Unit on L6 2" $
          EE.runEval (EE.eval
          (ERec (ESucc EZero)
          (ELam (Name "x") TBool
          (EApp (ELam (Name "x") TBool (ESucc EZero)) ETrue))
          (EApp (ELam (Name "x") TBool (EIf (EVar (Name "x")) (ESucc EZero) EZero)) ETrue))) EE.Emp @?= Just (VSuccN 1) -}
    ]
