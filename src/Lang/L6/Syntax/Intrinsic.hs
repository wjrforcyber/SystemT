{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lang.L6.Syntax.Intrinsic where

import Data.Kind (Type)
import qualified Lang.L6.Syntax.Extrinsic as E

data Ty
  = TNat
  | TBool
  | TUnit
  | TProd Ty Ty
  | TFun Ty Ty
  deriving (Eq, Show)

newtype Name = Name String
  deriving (Eq)

data Bind = Bind Name Ty
  deriving (Eq)

data Ctx
  = Emp
  | Snoc Ctx Bind
  deriving (Eq)

data In :: Ctx -> Bind -> Type where
  Here :: In ('Snoc ctx b) b
  There :: In ctx b -> In ('Snoc ctx b') b

data Exp :: Ctx -> Ty -> Type where
  EZero :: Exp ctx 'TNat
  ESucc :: Exp ctx 'TNat -> Exp ctx 'TNat
  ERec :: Exp ctx ty -> Exp ctx ('TFun ty ty) -> Exp ctx 'TNat -> Exp ctx ty
  ETrue :: Exp ctx 'TBool
  EFalse :: Exp ctx 'TBool
  EIf :: Exp ctx 'TBool -> Exp ctx ty -> Exp ctx ty -> Exp ctx ty
  EUnit :: Exp ctx 'TUnit
  ETuple :: Exp ctx ty1 -> Exp ctx ty2 -> Exp ctx ('TProd ty1 ty2)
  EFst :: Exp ctx ('TProd ty1 ty2) -> Exp ctx ty1
  ESnd :: Exp ctx ('TProd ty1 ty2) -> Exp ctx ty2
  EVar :: In ctx ('Bind name ty) -> Exp ctx ty
  ELam :: Exp ('Snoc ctx ('Bind name ty1)) ty2 -> Exp ctx ('TFun ty1 ty2)
  EApp :: Exp ctx ('TFun ty1 ty2) -> Exp ctx ty1 -> Exp ctx ty2

toExp :: Exp ctx ty -> E.Exp
toExp EZero = E.EZero
toExp (ESucc e) = E.ESucc (toExp e)
toExp ETrue = E.ETrue
toExp EFalse = E.EFalse
toExp (EIf e1 e2 e3) = E.EIf (toExp e1) (toExp e2) (toExp e3)
toExp EUnit = E.EUnit
toExp (ETuple e1 e2) = E.ETuple (toExp e1) (toExp e2)
toExp (EFst e) = E.EFst (toExp e)
toExp (ESnd e) = E.ESnd (toExp e)
toExp (EApp e1 e2) = E.EApp (toExp e1) (toExp e2)
toExp (ERec e1 e2 e3) = E.ERec (toExp e1) (toExp e2) (toExp e3)

instance Eq (Exp ctx ty) where
  e1 == e2 = toExp e1 == toExp e2

deriving instance Show (Exp ctx ty)

data Val :: Ty -> Type where
  VZero :: Val 'TNat
  VSucc :: Val 'TNat -> Val 'TNat
  VTrue :: Val 'TBool
  VFalse :: Val 'TBool
  VUnit :: Val 'TUnit
  VTuple :: Val ty1 -> Val ty2 -> Val ('TProd ty1 ty2)
  VLam :: Exp ('Snoc 'Emp ('Bind name ty1)) ty2 -> Val ('TFun ty1 ty2)

deriving instance Eq (Val ty)

deriving instance Show (Val ty)

wk :: Exp ctx ty -> Exp ('Snoc ctx ('Bind name ty)) ty
wk _ = error "wk"

subst :: Exp ctx ty1 -> Exp ('Snoc ctx ('Bind name ty1)) ty2 -> Exp ctx ty2
subst _ _ = error "subst"
