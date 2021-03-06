{-# LANGUAGE ScopedTypeVariables #-}

module Lang.L4.Tests (unitTests, propertyTests) where

import Data.Maybe
import Lang.L4.Eval.EEval as E
import Lang.L4.Eval.IEval as I
-- import Lang.L4.Syntax.Extrinsic as E
import Lang.L4.Syntax.Intrinsic as I
import Lang.L4.Typecheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

propertyTests :: TestTree
propertyTests = testGroup "L4 Property tests" [tcL4Props, evalL4Props]

tcL4Props :: TestTree
tcL4Props =
  testGroup
    "Bidi-typecheck"
    [ QC.testProperty "every well-typed expression can be inferred" $
        \(e :: TcTyExp) ->
          tcisSuccess (tcinfer (tcgetExp e)),
      QC.testProperty "every well-typed expression can be checked for its type" $
        \(e :: TcTyExp) ->
          case runTC (tcinfer (tcgetExp e)) of
            Right ty -> tcisSuccess $ tccheck (tcgetExp e) ty
            Left _ -> error $ "This cannot happen because" ++ show e ++ " is well-typed"
    ]

evalL4Props :: TestTree
evalL4Props =
  testGroup
    "eval"
    [ QC.testProperty "1-Well-typed expressions reduced to a value" $
        \(e :: TcTyExp) ->
          isJust (E.eval (tcgetExp e))
    ]

unitTests :: TestTree
unitTests = testGroup "L4 Unit tests" [unitL4Tests]

unitL4Tests :: TestTree
unitL4Tests =
  testGroup
    "L4"
    [ testCase "Unit on L4 0" $
        I.eval (I.EIf I.ETrue (I.ESucc I.EZero) (I.ESucc (I.ESucc I.EZero))) @?= I.VSuccN 1
    ]
