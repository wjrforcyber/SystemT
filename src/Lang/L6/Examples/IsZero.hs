{-# LANGUAGE OverloadedStrings #-}

-- | isZero
module Lang.L6.Examples.IsZero where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Base
import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QC

-- | isZero in Haskell
isZeroHs :: Nat -> Bool
isZeroHs Zero = True
isZeroHs (Succ _) = False

-- | type of isZero in L6
isZeroTy :: Ty
isZeroTy = TFun TNat TBool

-- | isZero in L6
isZeroExp :: Exp
isZeroExp =
  ELam
    "n"
    TNat
    ( ERec
        ETrue
        (ELam "b" TBool EFalse)
        (EVar "n")
    )

-- | check that both versions agree
isZeroProp :: QC.Property
isZeroProp = QC.property $ \n ->
  toBool (eval $ EApp isZeroExp (fromNat n)) === Just (isZeroHs n)

isZeroProg :: Program
isZeroProg = Program "isZero" isZeroTy isZeroExp isZeroProp
