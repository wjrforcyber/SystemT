-- | This is the interpreter for L1.

module Lang.L1.Eval where

import Common.Types
import Lang.L1.Syntax

eval :: Exp -> Nat
eval (ENat n) = n
eval (EAdd e1 e2) = eval e1 + eval e2
eval (EMul e1 e2) = eval e1 * eval e2
