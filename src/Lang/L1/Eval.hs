-- | This is the interpreter for L1.

module Lang.L1.Eval where

import Common.Types
import Lang.L1.Syntax


eval :: Exp -> Int
eval (ENat Zero) = 0
eval (ENat (Succ e)) = 1 + eval (ENat e)
eval (EAdd e1 e2) = eval e1 + eval e2
eval (EMul e1 e2) = eval e1 * eval e2

