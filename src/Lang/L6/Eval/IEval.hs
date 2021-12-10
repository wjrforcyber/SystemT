{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Lang.L6.Eval.IEval where
import Lang.L6.Syntax.Intrinsic

eval :: Exp 'Emp ty -> Val ty
eval EZero = VZero
eval (ESucc e) = VSucc (eval e)
eval (EIter e1 _ e3) =
  case eval e3 of
    VZero -> eval e1
    VSucc _ -> error "succ"
eval ETrue = VTrue
eval EFalse = VFalse
eval EUnit = VUnit
eval (ETuple e1 e2) = VTuple (eval e1) (eval e2)
eval (EFst e) =
  case eval e of
    VTuple v1 _ -> v1
eval (ESnd e) =
  case eval e of
    VTuple _ v2 -> v2
eval (EIf e1 e2 e3) =
  case eval e1 of
    VTrue -> eval e2
    VFalse -> eval e3
eval (EVar _) = error "variable"
eval (ELam e) = VLam e
eval (EApp e1 e2) =
  case eval e1 of
    VLam e' -> eval (subst e2 e')

evalStep :: Exp 'Emp ty -> Either (Exp 'Emp ty) (Val ty)
evalStep _ = error "evalStep"
