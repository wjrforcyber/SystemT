module Lang.L2.Typecheck where

import Lang.L2.Syntax

--checker
check :: Exp->Ty->Bool

check (ENat _) TNat = True
check (EBool _) TBool = True

check (EAdd e1 e2) TNat = check e1 TNat && check e2 TNat
check (EMul e1 e2) TNat = check e1 TNat && check e2 TNat

check (EIf e1 e2 e3) TNat = check e1 TBool && (check e2 TNat || check e3 TNat)
check (EIf e1 e2 e3) TBool = check e1 TBool && (check e2 TBool || check e3 TBool)

check _ _ = False


--infer
infer :: Exp->Maybe Ty
infer (ENat _) = Just TNat
infer (EBool _) = Just TBool
infer (EAdd e1 e2) = Just TNat
infer (EMul e1 e2) = Just TNat

infer (EIf (EBool True) e2 e3) = infer e2
infer (EIf (EBool False) e2 e3) = infer e3
infer (EIf e1 e2 e3)  = infer (EIf (EBool (check e1 TBool)) e2 e3)
infer _ = Nothing
