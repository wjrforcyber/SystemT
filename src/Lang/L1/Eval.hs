-- | This is the interpreter for L1.


module Lang.L1.Eval where

import Common.Types
import Lang.L1.Syntax

eval :: Exp -> Nat
eval (ENat n) = n
eval (EAdd e1 e2) = eval e1 + eval e2
eval (EMul e1 e2) = eval e1 * eval e2


opt :: Exp -> Exp
opt (ENat n) = ENat n

opt (EAdd (ENat e1) (ENat e2)) = ENat (e1+e2)

opt (EMul _ (ENat 0)) = ENat 0
opt (EMul e1 (ENat(Succ e2))) = EAdd (opt e1) (opt (EMul e1 (ENat e2)))

opt (EMul (ENat 0) _) = ENat 0
opt (EMul (ENat(Succ e1)) e2 ) = EAdd (opt (EMul (ENat e1) e2)) (opt e2)

opt (EAdd e1 e2) = EAdd (opt e1) (opt e2)

opt (EMul e1 e2) = opt (EMul (opt e1) (opt e2))


-- multiplication by repeated addition

optMul :: Nat -> Exp -> Exp
optMul Zero _ = ENat 0
optMul (Succ n) e2 = EAdd e2 (optMul n e2)

-- partially evaluate multiplication to addition

opt2 :: Exp -> Exp
opt2 e@(ENat _) = e
opt2 (EAdd e1 e2) = EAdd (opt2 e1) (opt2 e2)
opt2 (EMul e1 e2) = optMul (eval e1) (opt2 e2)



--checker
check :: Exp->Ty->Bool

check (ENat e1) Bool = Fasle
check (ENat e1) Nat = True
check (EBool e1) Bool = True
check (EBool e1) Nat = False

check (EAdd (ENat e1) (ENat e2)) Bool = False
check (EAdd (EBool e1) _) Bool = False
check (EAdd _ (EBool e2)) Bool = False

check (EMul (ENat e1) (ENat e2)) Bool = False
check (EMul (EBool e1) _) Bool = False
check (EMul _ (EBool e1)) Bool = False

check (EIf (ENat e1) e2 e3) Bool = False

-- Types of e1 and e2 are unknow in exp, so True or False...
check (EIf (EBool e1) e2 e3) Bool = True

check (EAdd (ENat e1) (ENat e2)) Nat = True
check (EAdd (EBool e1) _) Nat = False
check (EAdd _ (EBool e2)) Nat = False

check (EMul (ENat e1) (ENat e2)) Nat = True
check (EMul (EBool e1) _) Nat = False
check (EMul _ (EBool e1)) Nat = False

check (EIf (ENat e1) e2 e3) Nat = False



--inference

infer :: Exp->Maybe Ty

infer (ENat e) = Just Nat
infer (EBool e) = Just Bool

infer (EAdd (ENat e1) (ENat e2)) = Just Nat
infer (EMul (ENat e1) (ENat e2)) = Just Nat


infer (EAdd _ (EBool _)) = Nothing
infer (EAdd (EBool _) _) = Nothing

infer (EMul _ (EBool _)) = Nothing
infer (EMul (EBool _) _) = Nothing

infer (EIf (ENat _) e2 e3) = Nothing
infer (EIf (EBool True) e1 e2) = infer (e1)
infer (EIf (EBool False) e1 e2) = infer (e2)
