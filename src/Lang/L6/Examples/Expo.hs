{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Predecessor
module Lang.L6.Examples.Expo where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Base
import Lang.L6.Examples.Mul (mulExp)
import Test.QuickCheck as QC

-- | expo in Haskell
expoHs :: Nat -> Nat -> Nat
-- expoHs Zero Zero = Zero
expoHs _ Zero = Succ Zero
expoHs n (Succ m) = expoHs n m * n

-- | type of expo in L6
expoTy :: Ty
expoTy = TFun TNat (TFun TNat TNat)

expoExp :: Exp
expoExp =
  ELam
    "nat_n"
    TNat
    ( ELam
        "nat_m"
        TNat
        ( ERec
            (ESucc EZero)
            (EApp mulExp (EVar "nat_n"))
            (EVar "nat_m")
        )
    )

-- | check that both versions agree
expoProp :: Property
expoProp = property $
  -- forAll (arbitrary `suchThat` (<= 5)) $ \n m ->
  QC.withMaxSuccess 5 $ \n m ->
    toNat (evalStar $ EApp (EApp expoExp (fromNat n)) (fromNat m)) === Just (expoHs n m)

expoProg :: Program
expoProg = Program "expo" expoTy expoExp expoProp
