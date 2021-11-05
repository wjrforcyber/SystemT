{-# LANGUAGE GADTs #-}

-- | Evaluator for Extrinsic L4
module Lang.L4.Eval.IEval where

import Common.Types

import Lang.L4.Syntax.Intrinsic

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
eval EUnit = VUnit
eval (ETuple e1 e2) =
  case eval e1 of
    ty1 -> case eval e2 of
      ty2 -> VTuple ty1 ty2
eval (EFst e) =
  case eval e of
    VTuple (TProd v1 v2) -> v1
eval (ESnd e) =
  case eval e of
    VTuple (TProd v1 v2) -> v2
