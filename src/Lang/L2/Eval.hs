-- | Evaluator for L2
module Lang.L2.Eval where

import Lang.L2.Syntax

eval :: Exp -> Maybe Val
eval (ENat n) = Just (VNat n)
eval (EBool b) = Just (VBool b)
eval (EAdd e1 e2) =
  do
    VNat n <- eval e1
    VNat m <- eval e2
    return $ VNat (n + m)
eval (EMul e1 e2) =
  do
    VNat n <- eval e1
    VNat m <- eval e2
    return $ VNat (n * m)
eval (EIf e1 e2 e3) =
  do
    VBool b1 <- eval e1
    if b1
      then eval e2
      else eval e3
