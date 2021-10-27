-- | This is the Syntax of L2.
module Lang.L3.Syntax where

import Common.Types
import Test.QuickCheck
import Test.QuickCheck.Gen

data Ty
  = TNat
  | TBool
  deriving (Eq, Show)

data Exp
  = EZero
  | ESucc Exp
  | ETrue
  | EFalse
  | EAdd Exp Exp
  | EMul Exp Exp
  | EIf Exp Exp Exp
  deriving (Eq, Show)

data Val
  = VSuccN Nat
  | VTrue
  | VFalse
  deriving (Eq)

instance Arbitrary Ty where
  arbitrary = oneof [pure TNat, pure TBool]

instance Arbitrary Exp where
  arbitrary = sized arbExp

arbExp :: Int -> Gen Exp
arbExp 0 =
  oneof
    [ MkGen (\_ _ -> EZero),
      MkGen (\_ _ -> ETrue),
      MkGen (\_ _ -> EFalse)
    ]
arbExp n = do
  (Positive m) <- arbitrary
  let subExp = arbExp (n `div` (m + 1))
  oneof
    [ subExp,
      ESucc <$> subExp,
      EAdd <$> subExp <*> subExp,
      EMul <$> subExp <*> subExp,
      EIf <$> subExp <*> subExp <*> subExp
    ]
