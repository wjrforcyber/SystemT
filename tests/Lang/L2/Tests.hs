{-# LANGUAGE ScopedTypeVariables #-}

module Lang.L2.Tests (unitTests, propertyTests) where

import Common.Types
import Data.Either
import Data.Maybe
import Lang.L2.Eval
import Lang.L2.Syntax
import Lang.L2.Typecheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

propertyTests :: TestTree
propertyTests = testGroup "L2 Property tests" [tcL2Props, evalL2Props, l2Props]

tcL2Props :: TestTree
tcL2Props =
  testGroup
    "Bidi-typecheck"
    [ QC.testProperty "if a type can be inferred then it can be checked for the same type" $
        \(e :: TcTyExp) (ty :: Ty) ->
          if (tcinfer (tcgetExp e) == (return ty)) then (tccheck (tcgetExp e) (ty) == return ()) else True,
      QC.testProperty "if a type can be checked with nat, then it will also be inferred to nat" $
        \(e :: TcTyExp) (ty :: Ty) ->
          if (tccheck (tcgetExp e) (ty) == return ()) then (tcinfer (tcgetExp e) == (return ty)) else True,
      QC.testProperty "every well-typed expression can be inferred" $
        \(e :: TcTyExp) ->
          isRight (return (tcinfer (tcgetExp e)))
    ]

evalL2Props :: TestTree
evalL2Props =
  testGroup
    "eval"
    [ QC.testProperty "eval of Nat is itself" $
        \(x :: Nat) -> eval (ENat x) == Just (VNat x),
      QC.testProperty "eval of Add is +" $
        \(x :: Nat) (y :: Nat) ->
          eval (EAdd (ENat x) (ENat y)) == Just (VNat (x + y)),
      QC.testProperty "eval of Mul is *" $
        \(x :: Nat) (y :: Nat) ->
          eval (EMul (ENat x) (ENat y)) == Just (VNat (x * y)),
      QC.testProperty "eval of EIf True is *" $
        \(y :: Exp) (z :: Exp) ->
          eval (EIf (EBool True) y z) == eval y,
      QC.testProperty "eval of EIf False is *" $
        \(y :: Exp) (z :: Exp) ->
          eval (EIf (EBool False) y z) == eval z,
      QC.testProperty "Well-typed expressions reduced to a value" $
        \(e :: TyExp) ->
          isJust (eval (getExp e))
    ]

l2Props :: TestTree
l2Props =
  testGroup
    "L2"
    [ QC.testProperty "check test EAdd" $
        \(e1 :: Exp) (e2 :: Exp) ->
          check (EAdd e1 e2) TNat == (check e1 TNat && check e2 TNat),
      QC.testProperty "check test EMul" $
        \(e1 :: Exp) (e2 :: Exp) ->
          check (EMul e1 e2) TNat == (check e1 TNat && check e2 TNat),
      QC.testProperty "inferred type should be checked true" $
        \(e :: Exp) ->
          case infer e of
            Just ty -> check e ty
            Nothing -> True,
      QC.testProperty "checked type should be same as inferred type" $
        \(e :: Exp) (ty :: Ty) ->
          not (check e ty) || (infer e == Just ty)
    ]

unitTests :: TestTree
unitTests = testGroup "L2 Unit tests" [unitL2Tests]

unitL2Tests :: TestTree
unitL2Tests =
  testGroup
    "L2"
    [ testCase "Unit on L2 0" $
        check (EIf (EBool True) (ENat 1) (ENat 3)) TNat @?= True,
      testCase "Unit on L2 1" $
        check (EIf (EBool False) (ENat 2) (EAdd (ENat 1) (ENat 3))) TNat @?= True,
      testCase "Unit on L2 2" $
        check (EIf (EBool True) (EBool True) (EBool False)) TBool @?= True,
      testCase "Unit on L2 3" $
        check (EAdd (ENat 1) (ENat 2)) TNat @?= True,
      testCase "Unit on L2 4" $
        check (EAdd (EAdd (ENat 1) (ENat 2)) (EAdd (ENat 3) (ENat 4))) TNat @?= True,
      testCase "Unit on L2 5" $
        check (EAdd (ENat 1) (ENat 2)) TNat @?= True,
      testCase "Unit on L2 6" $
        check (ENat 1) TBool @?= False,
      testCase "Unit on L2 7" $
        check (EIf (ENat 3) (ENat 1) (ENat 2)) TNat @?= False,
      testCase "Unit on L2 infer 0" $
        infer (ENat 1) @?= Just TNat,
      testCase "Unit on L2 infer 1" $
        infer (EBool True) @?= Just TBool,
      testCase "Unit on L2 infer 2" $
        infer (EAdd (ENat 1) (ENat 2)) @?= Just TNat,
      testCase "Unit on L2 infer 3" $
        infer (EBool False) @?= Just TBool,
      testCase "Unit on L2 infer 4" $
        infer (EIf (EBool True) (EAdd (ENat 1) (ENat 2)) (EMul (ENat 3) (ENat 4))) @?= Just TNat,
      testCase "Unit on L2 infer 5" $
        infer (EIf (EIf (EBool True) (EBool True) (EBool True)) (EAdd (ENat 1) (ENat 2)) (EMul (ENat 3) (ENat 4))) @?= Just TNat,
      testCase "Unit on L2 infer 6" $
        infer (EIf (EIf (EBool True) (EBool False) (EBool True)) (ENat 5) (ENat 6)) @?= Just TNat,
      testCase "Unit on L2 eval 0" $
        eval (ENat 1) @?= Just (VNat 1),
      testCase "Unit on L2 eval 1" $
        eval (EAdd (EIf (EBool True) (ENat 1) (ENat 2)) (ENat 3)) @?= Just (VNat 4),
      testCase "Unit on L2 eval 2" $
        eval (EIf (EIf (EBool True) (EBool False) (EAdd (ENat 1) (ENat 2))) (ENat 3) (EAdd (ENat 4) (ENat 5))) @?= Just (VNat 9),
      testCase "Unit on L2 TC tcchecker 0" $
        tccheck (EAdd (EIf (EBool True) (ENat 1) (ENat 2)) (ENat 3)) TNat @?= TC (Right ()),
      testCase "Unit on L2 TC tcchecker 1" $
        tccheck (EIf (EIf (EBool True) (EBool True) (EBool False)) (ENat 3) (EAdd (ENat 4) (ENat 5))) TNat @?= TC (Right ()),
      testCase "Unit on L2 TC tcinfer 0" $
        tcinfer (EAdd (ENat 1) (ENat 2)) @?= TC (Right TNat),
      testCase "Unit on L2 TC tcinfer 1" $
        tcinfer (EIf (EIf (EBool True) (EBool False) (EBool True)) (ENat 5) (ENat 6)) @?= TC (Right TNat)
    ]
