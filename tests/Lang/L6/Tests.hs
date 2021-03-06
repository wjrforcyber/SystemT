{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lang.L6.Tests (unitTests, propertyTests, exampleTests) where

import Data.Maybe
import Lang.L6.Eval.EEval as EE
-- import Lang.L6.Eval.IEval as IE
import Lang.L6.Examples
import Lang.L6.Syntax.Extrinsic as E
-- import Lang.L6.Syntax.Intrinsic as I
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
    [ QC.testProperty "every generated expression is well-scoped" $
        QC.withMaxSuccess 1_000_000 $ -- test 1 million times
          \(e :: E.Exp) ->
            fv e === [],
      QC.testProperty "every inferable expression can be inferred" $
        QC.withMaxSuccess 1_000_000 $ -- test 1 million times
          \(e :: TcTyExp) ->
            tcisSuccess (tcinfer (tcgetExp e)),
      QC.testProperty "every inferable expression can be checked for its inferred type" $
        QC.withMaxSuccess 1_000_000 $ -- test 1 million times
          \(e :: TcTyExp) ->
            tcisSuccess (tccheck (tcgetExp e) (tcgetTy e)),
      QC.testProperty "every well-typed expression can be inferred" $
        QC.withMaxSuccess 1_000_000 $ -- test 1 million times
          \(e :: TyExp) ->
            tcisSuccess (tcinfer (getExp e)),
      QC.testProperty "every well-typed expression can be checked for its type" $
        QC.withMaxSuccess 1_000_000 $ -- test 1 million times
          \(e :: TyExp) ->
            tcisSuccess (tccheck (getExp e) (getTy e))
    ]

evalL6Props :: TestTree
evalL6Props =
  testGroup
    "eval"
    [ QC.testProperty "*Progress: Type-inferable expressions always reduce to a value" $
        QC.withMaxSuccess 1_000_000 $ -- test 1 million times
          QC.within 5_000_000 $ -- timeout after 5s
            \(e :: TcTyExp) ->
              EE.isVal (EE.evalStar (tcgetExp e)),
      QC.testProperty "*Type-preservation: Type-inferable expressions reduce to a value of the same type" $
        QC.withMaxSuccess 1_000_000 $ -- test 1 million times
          QC.within 5_000_000 $ -- timeout after 5s
            \(e :: TcTyExp) ->
              case runTC (tcinfer (tcgetExp e)) E.Emp of
                Right ty -> tcisSuccess $ tccheck (EE.evalStar (tcgetExp e)) ty
                Left _ -> error $ "This cannot happen because" ++ show e ++ " is well-typed",
      QC.testProperty "*Progress: Well-typed expressions always reduce to a value" $
        QC.withMaxSuccess 1_000_000 $ -- test 1 million times
          QC.within 5_000_000 $ -- timeout after 5s
            \(e :: TyExp) ->
              EE.isVal (EE.evalStar (getExp e)),
      QC.testProperty "*Type-preservation: Well-typed expressions reduce to a value of the same type" $
        QC.withMaxSuccess 1_000_000 $ -- test 1 million times
          QC.within 5_000_000 $ -- timeout after 5s
            \(e :: TyExp) ->
              tcisSuccess (tccheck (EE.evalStar (getExp e)) (getTy e)),
      QC.testProperty "Progress: Type-inferable expressions always reduce to a value" $
        QC.withMaxSuccess 1_000_000 $ -- test 1 million times
          QC.within 5_000_000 $ -- timeout after 5s
            \(e :: TcTyExp) ->
              isJust $ EE.runEval (EE.eval (tcgetExp e)) EE.Emp,
      QC.testProperty "Type-preservation: Type-inferable expressions reduce to a value of the same type" $
        QC.withMaxSuccess 1_000_000 $ -- test 1 million times
          QC.within 5_000_000 $ -- timeout after 5s
            \(e :: TcTyExp) ->
              case runTC (tcinfer (tcgetExp e)) E.Emp of
                Right ty -> tcisSuccess $ tccheck (tcgetExp e) ty
                Left _ -> error $ "This cannot happen because" ++ show e ++ " is well-typed",
      QC.testProperty "Progress: Well-typed expressions always reduce to a value" $
        QC.withMaxSuccess 1_000_000 $ -- test 1 million times
          QC.within 5_000_000 $ -- timeout after 5s
            \(e :: TyExp) ->
              isJust $ EE.runEval (EE.eval (getExp e)) EE.Emp,
      QC.testProperty "Type-preservation: Well-typed expressions reduce to a value of the same type" $
        QC.withMaxSuccess 1_000_000 $ -- test 1 million times
          QC.within 5_000_000 $ -- timeout after 5s
            \(e :: TyExp) ->
              tcisSuccess (tccheck (getExp e) (getTy e))
    ]

unitTests :: TestTree
unitTests = testGroup "L6 Unit tests" [unitL6Tests]

unitL6Tests :: TestTree
unitL6Tests =
  testGroup
    "L6"
    [ testCase "Unit on L6 check on Lambda" $
        tcisSuccess (tccheck (E.ELam (E.Name "x") E.TBool (E.EIf (E.EVar (E.Name "x")) (E.ESucc E.EZero) E.EZero)) (E.TFun E.TBool E.TNat)) @?= True,
      testCase "Unit on L6 infer on Lambda" $
        runTC (tcinfer (E.ELam (E.Name "x") E.TBool (E.EIf (E.EVar (E.Name "x")) (E.ESucc E.EZero) E.EZero))) E.Emp @?= Right (E.TFun E.TBool E.TNat)
        -- testCase "Unit on L6 0" $
        --   IE.eval (I.EIf I.ETrue (I.ESucc I.EZero) (I.ESucc (I.ESucc I.EZero))) @?= I.VSucc I.VZero
        {- testCase "Unit on L6 1" $
          EE.runEval (EE.eval (EApp (ELam (Name "x") TBool (EIf (EVar (Name "x")) (ESucc EZero) EZero)) ETrue)) EE.Emp @?= Just (VSuccN 1),
        testCase "Unit on L6 2" $
          EE.runEval (EE.eval
          (EIter (ESucc EZero)
          (ELam (Name "x") TBool
          (EApp (ELam (Name "x") TBool (ESucc EZero)) ETrue))
          (EApp (ELam (Name "x") TBool (EIf (EVar (Name "x")) (ESucc EZero) EZero)) ETrue))) EE.Emp @?= Just (VSuccN 1) -}
    ]

exampleTest :: Program -> TestTree
exampleTest Program {..} =
  testGroup
    progName
    [ testCase ("has type " ++ show progTy) $
        tcisSuccess (tccheck progExp progTy) @?= True,
      QC.testProperty
        "evaluates correctly"
        progProp
    ]

exampleTests :: TestTree
exampleTests =
  testGroup "L6" $ map exampleTest [isZeroProg, predProg, addProg, fibProg, mulProg, expoProg, tetProg, ackerProg, facProg, doubleProg]
