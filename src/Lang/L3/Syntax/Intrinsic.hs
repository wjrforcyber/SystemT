{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This is the Syntax of L3.
module Lang.L3.Syntax.Intrinsic where

import Common.Types
import Test.QuickCheck
import Data.Kind (Type)

data Ty
  = TNat
  | TBool
  deriving (Eq, Show)

data Exp :: Ty -> Type where
  EZero :: Exp 'TNat
  ESucc :: Exp 'TNat -> Exp 'TNat
  ETrue :: Exp 'TBool
  EFalse :: Exp 'TBool
  EAdd :: Exp 'TNat -> Exp 'TNat -> Exp 'TNat
  EMul :: Exp 'TNat -> Exp 'TNat -> Exp 'TNat
  EIf :: Exp 'TBool -> Exp ty -> Exp ty -> Exp ty


deriving instance Eq (Exp ty)

deriving instance Show (Exp ty)

data Val :: Ty -> Type where
  VSuccN :: Nat -> Val 'TNat
  VTrue :: Val 'TBool
  VFalse :: Val 'TBool

deriving instance Eq (Val ty)

deriving instance Show (Val ty)

instance Arbitrary Ty where
  arbitrary = oneof [pure TNat, pure TBool]
