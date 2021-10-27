-- | Some useful types.
module Common.L3Types where

import Numeric.Natural
import Prettyprinter
import Test.QuickCheck



data L3Nat = Zero | Succ L3Nat deriving (Eq, Ord)
data L3Bool = TTrue | FFalse deriving (Eq, Ord)

toBool :: L3Bool -> Bool
toBool TTrue = True
toBool FFalse = False

fromBool :: Bool -> L3Bool
fromBool True = TTrue
fromBool False = FFalse

toNatural :: L3Nat -> Natural
toNatural Zero = 0
toNatural (Succ n) = succ (toNatural n)

fromNatural :: Natural -> L3Nat
fromNatural 0 = Zero
fromNatural n = Succ (fromNatural (pred n))

instance Show L3Nat where
  showsPrec p = showsPrec p . toNatural

instance Read L3Nat where
  readsPrec p s = [(fromNatural x, r) | (x, r) <- readsPrec p s]

instance Num L3Nat where
  fromInteger = fromNatural . fromInteger

  Zero + m = m
  (Succ n) + m = Succ (n + m)

  Zero * _ = Zero
  (Succ n) * m = m + (n * m)

  abs = id

  signum Zero = Zero
  signum (Succ _) = Succ Zero

  negate = id

instance Arbitrary L3Nat where
  arbitrary = fromNatural <$> arbitrarySizedNatural `suchThat` \n -> n < 20
  shrink = fmap fromNatural . shrinkIntegral . toNatural

instance Pretty L3Nat where
  pretty = pretty . toNatural
