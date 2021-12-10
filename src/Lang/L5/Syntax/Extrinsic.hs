{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This is the Syntax of L6.
module Lang.L5.Syntax.Extrinsic where

import Common.Types (Nat)
import Control.DeepSeq
import Data.String
import GHC.Generics
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Prettyprinter.Render.String
import Test.QuickCheck

data Ty
  = TNat
  | TBool
  | TUnit
  | TProd Ty Ty
  | TFun Ty Ty
  deriving (Eq, Generic, NFData)

data Ctx
  = Emp
  | Snoc Ctx (Name, Ty)
  deriving (Eq, Show, Generic, NFData)

lookupCtx :: Name -> Ctx -> Maybe Ty
lookupCtx _ Emp = Nothing
lookupCtx x (Snoc ctx (y, ty))
  | x == y = Just ty
  | otherwise = lookupCtx x ctx

extendCtx :: Name -> Ty -> Ctx -> Ctx
extendCtx x ty ctx = Snoc ctx (x, ty)

newtype Name = Name String
  deriving newtype (Eq, Show, IsString, NFData)

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
  | EVar Name --variables
  | ELam Name Ty Exp --abstraction
  | EApp Exp Exp --application
  deriving (Eq, Generic, NFData)

data Val
  = VSuccN Nat
  | VTrue
  | VFalse
  | VUnit
  | VTuple Val Val
  | VLam Name Ty Exp
  deriving (Eq, Show, Generic, NFData)

instance Arbitrary Ty where
  arbitrary =
    oneof
      [ pure TNat,
        pure TBool,
        pure TUnit,
        TProd <$> arbitrary <*> arbitrary,
        TFun <$> arbitrary <*> arbitrary
      ]

instance Arbitrary Exp where
  arbitrary = sized arbExp

instance Arbitrary Name where
  arbitrary = Name <$> elements (map (: []) ['a' .. 'z'])

arbName :: Name -> Ctx -> Gen Name
arbName x Emp = pure x
arbName x (Snoc ctx' (y, _)) =
  oneof
    [ pure x,
      arbName y ctx'
    ]

arbExpCtx :: Int -> Ctx -> Gen Exp
arbExpCtx 0 ctx =
  case ctx of
    Emp ->
      elements
        [ EZero,
          ETrue,
          EFalse,
          EUnit
        ]
    Snoc ctx' (x, _) ->
      oneof
        [ pure EZero,
          pure ETrue,
          pure EFalse,
          pure EUnit,
          EVar <$> arbName x ctx'
        ]
arbExpCtx n ctx = do
  (Positive m) <- arbitrary
  let subExp = arbExpCtx (n `div` (m + 1)) ctx
  case ctx of
    Emp ->
      oneof
        [ ESucc <$> subExp,
          EAdd <$> subExp <*> subExp,
          EMul <$> subExp <*> subExp,
          EIf <$> subExp <*> subExp <*> subExp,
          ETuple <$> subExp <*> subExp,
          EFst <$> subExp,
          ESnd <$> subExp,
          do
            x <- arbitrary
            ty <- arbitrary
            ELam x ty <$> arbExpCtx (n `div` (m + 1)) (Snoc ctx (x, ty)),
          EApp <$> subExp <*> subExp
        ]
    Snoc ctx' (y, _) ->
      oneof
        [ ESucc <$> subExp,
          EAdd <$> subExp <*> subExp,
          EMul <$> subExp <*> subExp,
          EIf <$> subExp <*> subExp <*> subExp,
          ETuple <$> subExp <*> subExp,
          EFst <$> subExp,
          ESnd <$> subExp,
          EVar <$> arbName y ctx',
          do
            x <- arbitrary
            ty <- arbitrary
            ELam x ty <$> arbExpCtx (n `div` m + 1) (Snoc ctx (x, ty)),
          EApp <$> subExp <*> subExp
        ]

arbExp :: Int -> Gen Exp
arbExp n = arbExpCtx n Emp

instance PP.Pretty Ty where
  pretty = prettyTy

instance Show Ty where
  showsPrec _ = renderShowS . PP.layoutPretty PP.defaultLayoutOptions . PP.pretty

prettyTy :: Ty -> PP.Doc ann
prettyTy TNat = PP.pretty "ℕ"
prettyTy TBool = PP.pretty "𝟐"
prettyTy TUnit = PP.pretty "𝟏"
prettyTy (TProd ty1 ty2) = PP.pretty "(" <+> prettyTy ty1 <+> PP.pretty "×" <+> prettyTy ty2 <+> PP.pretty ")"
prettyTy (TFun ty1 ty2) = PP.pretty "(" <+> prettyTy ty1 <+> PP.pretty "→" <+> prettyTy ty2 <+> PP.pretty ")"

instance PP.Pretty Name where
  pretty (Name x) = PP.pretty x

instance PP.Pretty Exp where
  pretty = prettyExp

instance Show Exp where
  showsPrec _ = renderShowS . PP.layoutPretty PP.defaultLayoutOptions . PP.pretty

prettyExp :: Exp -> PP.Doc ann
prettyExp EZero = PP.pretty "0"
prettyExp (ESucc e) = PP.pretty "S" <+> PP.pretty "(" <+> prettyExp e <+> PP.pretty ")"
prettyExp ETrue = PP.pretty "true"
prettyExp EFalse = PP.pretty "false"
prettyExp (EAdd e1 e2) = PP.pretty "(" <+> prettyExp e1 <+> PP.pretty "+" <+> prettyExp e2 <+> PP.pretty ")"
prettyExp (EMul e1 e2) = PP.pretty "(" <+> prettyExp e1 <+> PP.pretty "*" <+> prettyExp e2 <+> PP.pretty ")"
prettyExp (EIf e1 e2 e3) = PP.pretty "if" <+> prettyExp e1 <+> PP.pretty "then" <+> prettyExp e2 <+> PP.pretty "else" <+> prettyExp e3
prettyExp EUnit = PP.pretty "()"
prettyExp (ETuple e1 e2) = PP.pretty "(" <+> prettyExp e1 <+> PP.pretty "," <+> prettyExp e2 <+> PP.pretty ")"
prettyExp (EFst e) = PP.pretty "fst" <+> PP.pretty "(" <+> prettyExp e <+> PP.pretty ")"
prettyExp (ESnd e) = PP.pretty "snd" <+> PP.pretty "(" <+> prettyExp e <+> PP.pretty ")"
prettyExp (EVar x) = PP.pretty x
prettyExp (ELam x ty e) = PP.pretty "λ(" <+> PP.pretty x <+> PP.pretty ":" <+> prettyTy ty <+> PP.pretty "). " <+> prettyExp e
prettyExp (EApp e1 e2) = PP.pretty "(" <+> prettyExp e1 <+> PP.pretty " " <+> prettyExp e2 <+> PP.pretty ")"
