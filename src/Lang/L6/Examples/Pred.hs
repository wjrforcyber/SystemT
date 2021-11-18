-- | Predecessor
module Lang.L6.Examples.Pred where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Base
import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QC

-- | pred in Haskell
predHs :: Nat -> Nat
predHs n =
  case n of
    Zero -> Zero
    Succ m -> m

-- | type of pred in L6
predTy :: Ty
predTy = TFun TNat TNat

-- | pred in L6
predExp :: Exp
predExp =
  ELam
    (Name "n")
    TNat
    ( ERec
        EZero
        (ELam (Name "m") TNat (EVar (Name "m")))
        (EVar (Name "n"))
    )

-- | check that both versions agree
predProp :: QC.Property
predProp = QC.property $ \n ->
  toNat (eval $ EApp predExp (fromNat n)) === Just (predHs n)

predProg :: Program
predProg = Program "pred" predTy predExp predProp
