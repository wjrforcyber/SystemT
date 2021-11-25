{-# LANGUAGE OverloadedStrings #-}

-- | Predecessor
module Lang.L6.Examples.Tet where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Base
import Lang.L6.Examples.Expo (expoExp, expoHs)
import Test.QuickCheck (Arbitrary (arbitrary), forAll, suchThat, (===))
import qualified Test.QuickCheck as QC

-- | Tetration in Haskell
tetHs :: Nat -> Nat -> Nat
-- tetHs Zero (Succ _) = Zero
tetHs _ Zero = Succ Zero
tetHs a (Succ n) = expoHs a (tetHs a n)

-- | type of tet in L6
tetTy :: Ty
tetTy = TFun TNat (TFun TNat TNat)

tetExp :: Exp
tetExp =
  ELam
    "nat_a"
    TNat
    ( ELam
        "nat_n"
        TNat
        ( ERec
            (ESucc EZero)
            (EApp expoExp (EVar "nat_a"))
            (EVar "nat_n")
        )
    )

-- | check that both versions agree
tetProp :: QC.Property
tetProp = QC.property $
  -- QC.withMaxSuccess 5 $
    forAll (arbitrary `suchThat` (<= 3)) $
      \a n ->
        toNat (eval $ EApp (EApp tetExp (fromNat a)) (fromNat n)) === Just (tetHs a n)

tetProg :: Program
tetProg = Program "tet" tetTy tetExp tetProp
