{-# LANGUAGE OverloadedStrings #-}

-- | Predecessor
module Lang.L6.Examples.Acker where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Base
import Test.QuickCheck (forAll, (===))
import qualified Test.QuickCheck as QC

-- | Tetration in Haskell
ackerHs :: Nat -> Nat -> Nat
ackerHs Zero n = Succ n
ackerHs (Succ m) Zero = ackerHs m (Succ Zero)
ackerHs (Succ m) (Succ n) = ackerHs m (ackerHs (Succ m) n)

-- | type of tet in L6
ackerTy :: Ty
-- ackerTy = TFun (TFun TNat TNat) TNat

ackerTy = TFun TNat (TFun TNat TNat)

compExp :: Exp
compExp =
  ELam
    "t"
    (TProd (TFun TNat TNat) (TFun TNat TNat))
    ( ELam
        "x"
        TNat
        (EApp (EFst (EVar "t")) (EApp (ESnd (EVar "t")) (EVar "x")))
    )

itExp :: Exp
itExp =
  ELam
    "nat_func_f"
    (TFun TNat TNat)
    ( ELam
        "nat_n"
        TNat
        ( ERec
            ( ELam
                "x"
                TNat
                (EVar "x")
            )
            ( ELam
                "nat_func_g"
                (TFun TNat TNat)
                (EApp compExp (ETuple (EVar "nat_func_f") (EVar "nat_func_g")))
            )
            (EVar "nat_n")
        )
    )

sExp :: Exp
sExp =
  ELam
    "m"
    TNat
    (ESucc (EVar "m"))

rExp :: Exp
rExp =
  ELam
    "f"
    (TFun TNat TNat)
    ( ELam
        "m"
        TNat
        ( EApp (EApp (EApp itExp (EVar "f")) (EVar "m")) (EApp (EVar "f") (ESucc EZero))
        )
    )

ackerExp :: Exp
ackerExp =
  ELam
    "n"
    TNat
    ( ERec
        sExp
        rExp
        (EVar "n")
    )

-- | check that both versions agree
ackerProp :: QC.Property
ackerProp = QC.property $
  forAll (QC.elements [0, 1, 2, 3]) $ \n ->
    forAll (QC.elements [0, 1, 2, 3, 4]) $ \m ->
      toNat (evalStar $ EApp (EApp ackerExp (fromNat n)) (fromNat m)) === Just (ackerHs n m)

ackerProg :: Program
ackerProg = Program "acker" ackerTy ackerExp ackerProp
