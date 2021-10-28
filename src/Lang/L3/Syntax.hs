{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies#-}
{-# LANGUAGE DataKinds #-}
-- | This is the Syntax of L3.
module Lang.L3.Syntax where

import Common.Types
import Test.QuickCheck

data Ty
  = TNat
  | TBool
  deriving (Eq, Show)

data Exp :: Ty -> * where
  EZero :: Exp TNat
  ESucc :: Exp TNat
  ETrue :: Exp TBool
  EFalse:: Exp TBool
  EAdd  :: Exp TNat -> Exp TNat -> Exp TNat
  EMul  :: Exp TNat -> Exp TNat -> Exp TNat
  EIf :: Exp TBool -> Exp ty -> Exp ty -> Exp ty
  deriving (Eq, Show)

data Val
  = VSuccN Nat
  | VTrue
  | VFalse
  deriving (Eq, Show)

instance Arbitrary Ty where
  arbitrary = oneof [pure TNat, pure TBool]

instance Arbitrary Exp where
  arbitrary = sized arbExp

arbExp :: Int -> Gen Exp
arbExp 0 =
  oneof
    [ pure EZero,
      pure ETrue,
      pure EFalse
    ]
arbExp n = do
  (Positive m) <- arbitrary
  let subExp = arbExp (n `div` (m + 1))
  oneof
    [ ESucc <$> subExp,
      EAdd <$> subExp <*> subExp,
      EMul <$> subExp <*> subExp,
      EIf <$> subExp <*> subExp <*> subExp
    ]
