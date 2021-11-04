-- | This is the Syntax of L5.
module Lang.L5.Syntax where

import Common.Types
import Test.QuickCheck

data Ty
  = TNat
  | TBool
  | TUnit
  | TProd Ty Ty
  | TFun Ty Ty
  deriving (Eq, Show)

newtype Name = Name String
  deriving (Eq, Show)

data Exp
  = EZero
  | ESucc Exp
  | ETrue
  | EFalse
  | EAdd Exp Exp
  | EMul Exp Exp
  | EIf Exp Exp Exp
  | EUnit
  | ETuple Exp Exp
  | EFst Exp
  | ESnd Exp
  | EVar Name
  | ELam Name Ty Exp
  | EApp Exp Exp
  deriving (Eq, Show)

data Val
  = VSuccN Nat
  | VTrue
  | VFalse
  | VUnit
  | VTuple Val Val
  | VLam Name Ty Val
  deriving (Eq, Show)

instance Arbitrary Ty where
  arbitrary =
    oneof
      [ pure TNat,
        pure TBool,
        pure TUnit,
        TProd <$> arbitrary <*> arbitrary
      ]

instance Arbitrary Exp where
  arbitrary = sized arbExp

arbExp :: Int -> Gen Exp
arbExp 0 =
  oneof
    [ pure EZero,
      pure ETrue,
      pure EFalse,
      pure EUnit
    ]
arbExp n = do
  (Positive m) <- arbitrary
  let subExp = arbExp (n `div` (m + 1))
  oneof
    [ ESucc <$> subExp,
      EAdd <$> subExp <*> subExp,
      EMul <$> subExp <*> subExp,
      EIf <$> subExp <*> subExp <*> subExp,
      ETuple <$> subExp <*> subExp,
      EFst <$> subExp,
      ESnd <$> subExp
    ]
