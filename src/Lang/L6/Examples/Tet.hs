{-# LANGUAGE OverloadedStrings #-}

-- | Predecessor
module Lang.L6.Examples.Tet where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Base
import Lang.L6.Examples.Expo ( expoHs, expoExp )
import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QC

-- | Tetration in Haskell
tetHs :: Nat -> Nat -> Nat
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
  QC.withMaxSuccess 10 $ -- test 5 times
    \a n ->
      toNat (eval $ EApp (EApp expoExp (fromNat a)) (fromNat n)) === Just (tetHs a n)

tetProg :: Program
tetProg = Program "tet" tetTy tetExp tetProp
