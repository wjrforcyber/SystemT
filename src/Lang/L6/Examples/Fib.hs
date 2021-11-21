{-# LANGUAGE OverloadedStrings #-}

-- | Predecessor
module Lang.L6.Examples.Fib where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Base
import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QC

-- | pred in Haskell
fibHs :: Nat -> Nat
fibHs Zero = Zero
fibHs (Succ Zero) = Succ Zero
fibHs (Succ (Succ n)) = fibHs (Succ n) + fibHs n

-- | type of pred in L6
fibTy :: Ty
fibTy = TFun TNat TNat

-- | pred in L6
-- fibExp :: Exp -> Exp
-- fibExp EZero = EZero
-- fibExp (ESucc EZero) = (ESucc EZero)
-- fibExp (ESucc (ESucc e)) = ERec(fibExp e, ,ESucc e )
fibExp :: Exp
fibExp = EZero

--TODO

-- | check that both versions agree
fibProp :: QC.Property
fibProp = QC.property $ \n ->
  toNat (eval $ EApp fibExp (fromNat n)) === Just (fibHs n)

fibProg :: Program
fibProg = Program "fib" fibTy fibExp fibProp
