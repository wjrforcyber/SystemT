{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This is the Syntax of L4.
module Lang.L4.Syntax.Intrinsic where

import Common.Types
import Data.Kind (Type)
import Test.QuickCheck

data Ty
  = TNat
  | TBool
  | TUnit
  | TProd Ty Ty
  deriving (Eq, Show)

data Exp :: Ty -> Type where
  EZero :: Exp 'TNat
  ESucc :: Exp 'TNat -> Exp 'TNat
  ETrue :: Exp 'TBool
  EFalse :: Exp 'TBool
  EAdd :: Exp 'TNat -> Exp 'TNat -> Exp 'TNat
  EMul :: Exp 'TNat -> Exp 'TNat -> Exp 'TNat
  EIf :: Exp 'TBool -> Exp ty -> Exp ty -> Exp ty
  EUnit :: Exp 'TUnit
  ETuple :: Exp ty1 -> Exp ty2 -> Exp ('TProd ty1 ty2)
  EFst :: Exp ('TProd ty1 ty2) -> Exp ty
  ESnd :: Exp ('TProd ty1 ty) -> Exp ty

deriving instance Eq (Exp ty)

deriving instance Show (Exp ty)

data Val :: Ty -> Type where
  VSuccN :: Nat -> Val 'TNat
  VTrue :: Val 'TBool
  VFalse :: Val 'TBool
  VUnit :: Val 'TUnit
  VTuple :: Val ('TProd ty1 ty2)
  VFst :: Val ('TProd ty1 ty2) -> Val ty1
  VSnd :: Val ('TProd ty1 ty2) -> Val ty2

deriving instance Eq (Val ty)

deriving instance Show (Val ty)

instance Arbitrary Ty where
  arbitrary =
    oneof
      [ pure TNat,
        pure TBool,
        pure TUnit,
        TProd <$> arbitrary <*> arbitrary
      ]
