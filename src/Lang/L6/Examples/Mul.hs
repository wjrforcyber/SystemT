{-# LANGUAGE OverloadedStrings #-}

-- | Predecessor
module Lang.L6.Examples.Mul where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Add (addExp)
import Lang.L6.Examples.Base
import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QC

-- | mul in Haskell
mulHs :: Nat -> Nat -> Nat
mulHs Zero _ = Zero
mulHs (Succ n) m = n * m + m

-- | type of mul in L6
mulTy :: Ty
mulTy = TFun TNat (TFun TNat TNat)

mulExp :: Exp
mulExp =
  ELam
    "nat_n"
    TNat
    ( ELam
        "nat_m"
        TNat
        ( ERec
            EZero
            (EApp addExp (EVar "nat_m"))
            (EVar "nat_n")
        )
    )

-- | check that both versions agree
mulProp :: QC.Property
mulProp = QC.property $ \n m ->
  toNat (eval $ EApp (EApp mulExp (fromNat n)) (fromNat m)) === Just (mulHs n m)

mulProg :: Program
mulProg = Program "mul" mulTy addExp mulProp
