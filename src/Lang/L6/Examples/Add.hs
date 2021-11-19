{-# LANGUAGE OverloadedStrings #-}

-- | Predecessor
module Lang.L6.Examples.Add where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Base
import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QC

-- | pred in Haskell
addHs :: Nat -> Nat -> Nat
addHs Zero n = n
addHs (Succ n) m = Succ (n + m)

-- | type of add in L6
addTy :: Ty
addTy = TFun TNat TNat

-- | pred in L6
-- Use ERec to decrease e3 and finally add to e1, e2 is Succ which iteratively apply to Rec, fianlly apply to e1
addExp :: Exp
addExp =
  ELam
    "n"
    (TProd TNat TNat)
    ( ERec
        (EFst (EVar "n"))
        ( ELam
            "t"
            TNat
            (ESucc (ESnd (EVar "t")))
        )
        (EVar "n")
    )

-- | check that both versions agree
addProp :: QC.Property
addProp = QC.property $ \n m ->
  toNat (eval $ EApp addExp (ETuple (fromNat n) (fromNat m))) === Just (addHs n m)

addProg :: Program
addProg = Program "add" addTy addExp addProp
