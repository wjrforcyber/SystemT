-- | Evaluator for Extrinsic L4
module Lang.L4.Eval.EEval where

import Lang.L4.Syntax.Extrinsic

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
eval EUnit = Just VUnit
eval (ETuple e1 e2) =
  do
    n <- eval e1
    m <- eval e2
    return $ VTuple n m
eval (EFst e) =
  do
    VTuple v1 _ <- eval e
    return v1
eval (ESnd e) =
  do
    VTuple _ v2 <- eval e
    return v2
