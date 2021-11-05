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
import qualified Lang.L4.Syntax.Extrinsic as E

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
  EFst :: Exp ('TProd ty1 ty2) -> Exp ty1
  ESnd :: Exp ('TProd ty1 ty2) -> Exp ty2

toExp :: Exp ty -> E.Exp
toExp EZero = E.EZero
toExp (ESucc e) = E.ESucc (toExp e)
toExp ETrue = E.ETrue
toExp EFalse = E.EFalse
toExp (EAdd e1 e2) = E.EAdd (toExp e1) (toExp e2)
toExp (EMul e1 e2) = E.EMul (toExp e1) (toExp e2)
toExp (EIf e1 e2 e3) = E.EIf (toExp e1) (toExp e2) (toExp e3)
toExp EUnit = E.EUnit
toExp (ETuple e1 e2) = E.ETuple (toExp e1) (toExp e2)
toExp (EFst e) = E.EFst (toExp e)
toExp (ESnd e) = E.ESnd (toExp e)

instance Eq (Exp ty) where
  e1 == e2 = toExp e1 == toExp e2

deriving instance Show (Exp ty)

data Val :: Ty -> Type where
  VSuccN :: Nat -> Val 'TNat
  VTrue :: Val 'TBool
  VFalse :: Val 'TBool
  VUnit :: Val 'TUnit
  VTuple :: Val ty1 -> Val ty2 -> Val ('TProd ty1 ty2)

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
