-- | This is the Syntax of L2.
module Lang.L2.Syntax where

import Common.Types
import Test.QuickCheck

data Ty
  = TNat
  | TBool
  deriving (Eq, Show)

data Exp
  = ENat Nat
  | EBool Bool
  | EAdd Exp Exp
  | EMul Exp Exp
  | EIf Exp Exp Exp
  deriving (Eq, Show)

data Val
  = VNat Nat
  | VBool Bool

instance Arbitrary Ty where
  arbitrary = oneof [pure TNat, pure TBool]

instance Arbitrary Exp where
  arbitrary = sized arbExp

arbExp :: Int -> Gen Exp
arbExp 0 =
  oneof
    [ ENat <$> arbitrary `suchThat` \n -> n < 30,
      EBool <$> arbitrary
    ]
arbExp n = do
  (Positive m) <- arbitrary
  let subExp = arbExp (n `div` (m + 1))
  oneof
    [ subExp,
      EAdd <$> subExp <*> subExp,
      EMul <$> subExp <*> subExp,
      EIf <$> subExp <*> subExp <*> subExp
    ]
