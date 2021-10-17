module Lang.L2.Typecheck where

import Lang.L2.Syntax

--checker

check :: Exp->Ty->Bool

check (ENat _) TNat = True
check (EBool _) TBool = True

check (EAdd e1 e2) TNat = check e1 TNat && check e2 TNat
check (EMul e1 e2) TNat = check e1 TNat && check e2 TNat

-- check (EIf e1 e2 e3) TNat =  if check e1 TBool then if e1 == (EBool True) then check e2 TNat else check e3 TNat else False
-- check (EIf e1 e2 e3) TBool = if check e1 TBool then if e1 == (EBool True) then check e2 TBool else check e3 TBool else False

check (EIf e1 e2 e3) TNat =  if check e1 TBool then (check e2 TNat && check e3 TNat) else False
check (EIf e1 e2 e3) TBool =  if check e1 TBool then (check e2 TBool && check e3 TBool) else False

check _ _ = False


--infer

infer :: Exp->Maybe Ty

infer (ENat _) = Just TNat
infer (EBool _) = Just TBool

infer (EAdd e1 e2) = if (infer e1 == Just TNat) && (infer e2 == Just TNat) then Just TNat else Nothing
infer (EMul e1 e2) = if (infer e1 == Just TNat) && (infer e2 == Just TNat) then Just TNat else Nothing

infer (EIf e1 e2 e3) = if (infer e1 == Just TBool) && (infer e2 == infer e3) then infer e2 else Nothing

-- infer (EIf e1 e2 e3) = if check e1 TBool then if e1 == (EBool True) then infer e2 else infer e3 else Nothing
