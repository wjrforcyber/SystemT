-- | This is the interpreter for L1.

module Lang.L1.Eval where

import Common.Types
import Lang.L1.Syntax


eval :: Exp -> Nat
eval (ENat n) = n
eval (EAdd (ENat e1) (ENat e2)) = eval (ENat e1) + eval (ENat e2)
eval (EMul (ENat e1) (ENat e2)) = eval (ENat e1) * eval (ENat e2)

