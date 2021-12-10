-- | This is the Syntax of L4.
module Lang.L4.Syntax.Extrinsic where

import Common.Types
import Test.QuickCheck

data Ty
  = TNat
  | TBool
  | TUnit
  | TProd Ty Ty
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
  deriving (Eq, Show)

data Val
  = VSuccN Nat
  | VTrue
  | VFalse
  | VUnit
  | VTuple Val Val
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
