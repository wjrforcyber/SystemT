module Lang.L2.Typecheck where

import Common.Types
import Lang.L2.Syntax

--checker
check :: Exp->Ty->Bool

check (ENat Zero) Nat = True
check (ENat (Succ(e))) Nat = True
check (EBool True) Bool = True
check (EBool False) Bool = True


check (EAdd (ENat e1) (ENat e2)) Nat = True
check (EMul (ENat e1) (ENat e2)) Nat = True

-- check (EIf (EBool e1) (Ty e2) (Ty e3)) Ty = True
check (EIf (EBool True) e1 e2) Nat = check e1 Nat
check (EIf (EBool False) e1 e2) Nat = check e2 Nat
check (EIf (EBool True) e1 e2) Bool = check e1 Bool
check (EIf (EBool False) e1 e2) Bool = check e2 Bool


check (EAdd e1 e2) Nat = (check e1 Nat) && (check e2 Nat)
check (EMul e1 e2) Nat = (check e1 Nat) && (check e2 Nat)

check (EIf e1 e2 e3) Nat = (check e1 Bool)
check (EIf e1 e2 e3) Bool = (check e1 Bool)

check _ Nat = False
check _ Bool = False
