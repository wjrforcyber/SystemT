{-# LANGUAGE OverloadedStrings #-}

-- | Predecessor
module Lang.L6.Examples.Fac where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Base
import Lang.L6.Examples.Mul (mulExp)
import Test.QuickCheck (forAll, (===))
import qualified Test.QuickCheck as QC

-- | mul in Haskell
facHs :: Nat -> Nat
facHs Zero = Succ Zero
facHs (Succ n) = Succ n * facHs n

-- | type of mul in L6
facTy :: Ty
facTy = TFun TNat TNat

facExp2 :: Exp
facExp2 =
  ELam
    "fac_n"
    TNat
    ( ERec
        (ETuple EZero (ESucc EZero))
        ( ELam
            "fac_t"
            (TProd TNat TNat)
            ( ETuple
                (ESucc (EFst (EVar "fac_t")))
                (EApp (EApp mulExp (ESucc (EFst (EVar "fac_t")))) (ESnd (EVar "fac_t")))
            )
        )
        (EVar "fac_n")
    )

facExp :: Exp
facExp =
  ELam
    "fac_m"
    TNat
    (ESnd (EApp facExp2 (EVar "fac_m")))

-- | check that both versions agree
facProp :: QC.Property
facProp = QC.property $
  forAll (QC.elements [0, 1, 2, 3, 4, 5]) $ \n ->
    toNat (evalStar $ EApp facExp (fromNat n)) === Just (facHs n)

facProg :: Program
facProg = Program "fac" facTy facExp facProp
