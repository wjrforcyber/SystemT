-- | Evaluator for L3
module Lang.L3.Eval where

import Lang.L3.Syntax.Extrinsic
import Lang.L3.Syntax.Intrinsic

eval :: Exp -> Maybe Val
eval EZero = Just (VSuccN 0)
eval (ESucc e) =
  do
    VSuccN n <- eval e
    return $ VSuccN (1 + n)
eval ETrue = Just VTrue
eval EFalse = Just VFalse
eval (EAdd e1 e2) =
  do
    VSuccN n <- eval e1
    VSuccN m <- eval e2
    return $ VSuccN (n + m)
eval (EMul e1 e2) =
  do
    VSuccN n <- eval e1
    VSuccN m <- eval e2
    return $ VSuccN (n * m)
eval (EIf e1 e2 e3) =
  do
    b1 <- eval e1
    if b1 == VTrue
      then eval e2
      else eval e3

evali :: Exp Ty -> Val
evali (EZero ENat) = VSuccN 0
evali (ESucc e ENat) =
  do
    VSuccN n <- eval e
    return $ VSuccN (1 + n)
evali (ETrue EBool) = VTrue
evali (EFalse EBool) = VFalse
evali (EAdd e1 e2 ENat) =
  do
    VSuccN n <- eval e1
    VSuccN m <- eval e2
    return $ VSuccN (n + m)
evali (EMul e1 e2 ENat) =
  do
    VSuccN n <- eval e1
    VSuccN m <- eval e2
    return $ VSuccN (n * m)
evali (EIf e1 e2 e3 ty) =
  do
    b1 <- eval e1
    if b1 == VTrue
      then eval e2
      else eval e3
