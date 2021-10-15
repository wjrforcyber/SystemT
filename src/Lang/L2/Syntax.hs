-- | This is the Syntax of L2.
module Lang.L2.Syntax where

import Common.Types


data Ty = Nat | Bool deriving (Eq, Ord)

data Exp
  = ENat Nat
  | EBool Bool
  | EAdd Exp Exp
  | EMul Exp Exp
  | EIf Exp Exp Exp
  deriving (Eq, Show)
