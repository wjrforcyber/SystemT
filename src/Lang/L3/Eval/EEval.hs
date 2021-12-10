-- | Evaluator for Extrinsic L3
module Lang.L3.Eval.EEval where

import Lang.L3.Syntax.Extrinsic

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
    case b1 of
      VTrue -> eval e2
      VFalse -> eval e3
      _ -> fail (show e1 ++ "has a type of" ++ show b1)
