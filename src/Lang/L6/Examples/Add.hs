{-# LANGUAGE OverloadedStrings #-}

-- | Predecessor
module Lang.L6.Examples.Add where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Base
import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QC

-- | add in Haskell
addHs :: Nat -> Nat -> Nat
addHs Zero n = n
addHs (Succ n) m = Succ (n + m)

-- | type of add in L6
addTy :: Ty
addTy = TFun TNat (TFun TNat TNat)

addExp :: Exp
addExp =
  ELam
    "nat_n"
    TNat
    ( ELam
        "nat_m"
        TNat
        ( ERec
            (EVar "nat_m")
            ( ELam
                "nat_t"
                TNat
                (ESucc (EVar "nat_t"))
            )
            (EVar "nat_n")
        )
    )

-- | check that both versions agree
addProp :: QC.Property
addProp = QC.property $ \n m ->
  toNat (eval $ EApp (EApp addExp (fromNat n)) (fromNat m)) === Just (addHs n m)

addProg :: Program
addProg = Program "add" addTy addExp addProp
