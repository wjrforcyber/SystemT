{-# LANGUAGE GADTs #-}

-- | Evaluator for Intrinsic L3
module Lang.L3.Eval.IEval where

import Common.Types
import Lang.L3.Syntax.Intrinsic

eval :: Exp ty -> Val ty
eval EZero = VSuccN 0
eval (ESucc e) =
  case eval e of
    VSuccN n -> VSuccN (Succ n)
eval ETrue = VTrue
eval EFalse = VFalse
eval (EAdd e1 e2) =
  case eval e1 of
    VSuccN n1 -> case eval e2 of
      VSuccN n2 -> VSuccN (n1 + n2)
eval (EMul e1 e2) =
  case eval e1 of
    VSuccN n1 -> case eval e2 of
      VSuccN n2 -> VSuccN (n1 * n2)
eval (EIf e1 e2 e3) =
  case eval e1 of
    VTrue -> eval e2
    VFalse -> eval e3
