{-# LANGUAGE OverloadedStrings #-}

-- | Predecessor
module Lang.L6.Examples.Pred where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Base
import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QC

-- | pred in Haskell
predHs :: Nat -> Nat
predHs Zero = Zero
predHs (Succ n) = n

-- | type of pred in L6
predTy :: Ty
predTy = TFun TNat TNat

-- | pred in L6
predExp :: Exp
predExp =
  ELam
    "n"
    TNat
    ( EFst
        ( EIter
            (ETuple EZero EZero)
            ( ELam
                "t"
                (TProd TNat TNat)
                (ETuple (ESnd (EVar "t")) (ESucc (ESnd (EVar "t"))))
            )
            (EVar "n")
        )
    )

-- | check that both versions agree
predProp :: QC.Property
predProp = QC.property $ \n ->
  toNat (evalStar $ EApp predExp (fromNat n)) === Just (predHs n)

predProg :: Program
predProg = Program "pred" predTy predExp predProp
