{-# LANGUAGE OverloadedStrings #-}

-- | Predecessor
module Lang.L6.Examples.Acker where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Base
import Lang.L6.Examples.Pred (predExp)
import Test.QuickCheck (Arbitrary (arbitrary), forAll, suchThat, (===))
import qualified Test.QuickCheck as QC

-- | Tetration in Haskell
ackerHs :: Nat -> Nat -> Nat
ackerHs Zero n = Succ n
ackerHs (Succ m) Zero = ackerHs m (Succ Zero)
ackerHs (Succ m) (Succ n) = ackerHs m (ackerHs (Succ m) n)

-- | type of tet in L6
ackerTy :: Ty
ackerTy = TFun TNat (TFun TNat TNat)

ackerExp :: Exp
ackerExp =
  ELam
    "nat_m"
    TNat
    ( ERec
        (ESucc (EVar "nat_n"))
        ( ELam
            "nat_n"
            TNat
            ( ERec
                (EApp (EApp ackerExp (EApp predExp (EVar "nat_m"))) (ESucc EZero))
                (EApp (EApp ackerExp (EApp predExp (EVar "nat_m"))) (EApp (EApp ackerExp (EVar "nat_m")) (EApp predExp (EVar "nat_n"))))
                (EVar "nat_n")
            )
        )
        (EVar "nat_m")
    )

-- | check that both versions agree
ackerProp :: QC.Property
ackerProp = QC.property $
  -- QC.withMaxSuccess 5 $
  forAll (arbitrary `suchThat` (<= 2)) $
    \m n ->
      toNat (eval $ EApp (EApp ackerExp (fromNat m)) (fromNat n)) === Just (ackerHs m n)

ackerProg :: Program
ackerProg = Program "acker" ackerTy ackerExp ackerProp
