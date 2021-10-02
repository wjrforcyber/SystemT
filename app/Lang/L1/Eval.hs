-- | This is the interpreter for L1.

module Lang.L1.Eval where

import Lang.L1.Syntax

eval :: Exp -> Int
eval Zero = 0
eval (Succ e) = 1 + eval e
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
