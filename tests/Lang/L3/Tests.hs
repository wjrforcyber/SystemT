{-# LANGUAGE ScopedTypeVariables #-}

module Lang.L3.Tests (propertyTests) where

import Data.Maybe
import Lang.L3.Eval
import Lang.L3.Syntax.Extrinsic
import Lang.L3.Syntax.Intrinsic
import Lang.L3.Typecheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC

propertyTests :: TestTree
propertyTests = testGroup "L3 Property tests" [tcL3Props, evalL3Props]

tcL3Props :: TestTree
tcL3Props =
  testGroup
    "Bidi-typecheck"
    [ QC.testProperty "if a type can be checked with nat, then it will also be inferred to nat" $
        \(e :: Exp) ->
          tccheck e TNat /= return () || (tcinfer e == return TNat),
      QC.testProperty "if a type can be checked with bool, then it will also be inferred to bool" $
        \(e :: Exp) ->
          tccheck e TBool /= return () || (tcinfer e == return TBool),
      QC.testProperty "every well-typed expression can be inferred" $
        \(e :: TcTyExp) ->
          tcisSuccess (tcinfer (tcgetExp e)),
      QC.testProperty "every well-typed expression can be checked for its type" $
        \(e :: TcTyExp) ->
          case runTC (tcinfer (tcgetExp e)) of
            Right ty -> tcisSuccess $ tccheck (tcgetExp e) ty
            Left _ -> error $ "This cannot happen because" ++ show e ++ " is well-typed"
    ]

evalL3Props :: TestTree
evalL3Props =
  testGroup
    "eval"
    [ QC.testProperty "1-Well-typed expressions reduced to a value" $
        \(e :: TcTyExp) ->
          isJust (eval (tcgetExp e)),
      QC.testProperty "2-Well-typed expressions reduced to a value" $
        \(e :: TcTyExp) ->
          isJust (evali (tcgetExp e))
    ]
