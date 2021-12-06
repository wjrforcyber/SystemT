{-# LANGUAGE OverloadedStrings #-}

-- | Predecessor
module Lang.L6.Examples.Double where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Add (addHs)
import Lang.L6.Examples.Base
import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QC

-- | Double in Haskell
doubleHs :: Nat -> Nat
doubleHs Zero = Zero
doubleHs n = addHs n n

-- | type of Double in L6
doubleTy :: Ty
doubleTy = TFun TNat TNat

doubleExp :: Exp
doubleExp =
  ELam
    "nat_n"
    TNat
    ( ERec
        (EVar "nat_n")
        ( ELam
            "nat_t"
            TNat
            (ESucc (EVar "nat_t"))
        )
        (EVar "nat_n")
    )

-- | check that both versions agree
doubleProp :: QC.Property
doubleProp = QC.property $ \n ->
  toNat (evalStar $ EApp doubleExp (fromNat n)) === Just (doubleHs n)

doubleProg :: Program
doubleProg = Program "double" doubleTy doubleExp doubleProp
