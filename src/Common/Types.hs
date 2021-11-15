{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | Some useful types.
module Common.Types where

import Numeric.Natural
import Prettyprinter
import Test.QuickCheck
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data Nat = Zero | Succ Nat deriving (Eq, Ord, Generic, NFData)

toNatural :: Nat -> Natural
toNatural Zero = 0
toNatural (Succ n) = succ (toNatural n)

fromNatural :: Natural -> Nat
fromNatural 0 = Zero
fromNatural n = Succ (fromNatural (pred n))


instance Show Nat where
  showsPrec p = showsPrec p . toNatural

instance Read Nat where
  readsPrec p s = [(fromNatural x, r) | (x, r) <- readsPrec p s]

instance Num Nat where
  fromInteger = fromNatural . fromInteger

  Zero + m = m
  (Succ n) + m = Succ (n + m)

  Zero * _ = Zero
  (Succ n) * m = m + (n * m)

  abs = id

  signum Zero = Zero
  signum (Succ _) = Succ Zero

  negate = id

instance Arbitrary Nat where
  arbitrary = fromNatural <$> arbitrarySizedNatural `suchThat` \n -> n < 20
  shrink = fmap fromNatural . shrinkIntegral . toNatural

instance Pretty Nat where
  pretty = pretty . toNatural
