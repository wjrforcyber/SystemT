module Lang.L2.Typecheck where

import Lang.L2.Syntax

--checker
check :: Exp->Ty->Bool

check (ENat _) Nat = True
check (EBool _) Bool = True

check (EAdd e1 e2) Nat = check e1 Nat && check e2 Nat
check (EMul e1 e2) Nat = check e1 Nat && check e2 Nat

check (EIf e1 e2 e3) Nat = check e1 Bool && (check e2 Nat || check e3 Nat)
check (EIf e1 e2 e3) Bool = check e1 Bool && (check e2 Bool || check e3 Bool)

check _ _ = False


--infer
infer :: Exp->Maybe Ty
infer (ENat _) = Just Nat
infer (EBool _) = Just Bool
infer (EAdd e1 e2) = Just Nat
infer (EMul e1 e2) = Just Nat

infer (EIf (EBool True) e2 e3) = infer e2
infer (EIf (EBool False) e2 e3) = infer e3
infer (EIf e1 e2 e3)  = infer (EIf (EBool (check e1 Bool)) e2 e3)
infer _ = Nothing
