-- | This is the Syntax of L1.

module Lang.L1.Syntax where

data Exp = Zero
         | Succ Exp
         | Add Exp Exp
         | Mul Exp Exp
  deriving (Eq, Show)
