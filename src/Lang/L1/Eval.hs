-- | This is the interpreter for L1.

module Lang.L1.Eval where

import Common.Types
import Lang.L1.Syntax

eval :: Exp -> Nat
eval (ENat n) = n
eval (EAdd e1 e2) = eval e1 + eval e2
eval (EMul e1 e2) = eval e1 * eval e2

-- This is only bounded to Nat
opt :: Exp -> Exp
opt (ENat n) = ENat n
opt (EAdd e1 e2) = EAdd e1 e2
opt (EMul e1 (ENat 0)) = ENat 0
opt (EMul e1 (ENat(Succ(e2)))) = EAdd (e1) (opt (EMul (e1) (ENat e2)))
opt (EMul e1 e2) = EMul e1 e2