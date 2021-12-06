{-# LANGUAGE OverloadedStrings #-}

-- | Predecessor
module Lang.L6.Examples.Fib where

import Lang.L6.Eval.EEval
import Lang.L6.Examples.Add (addExp)
import Lang.L6.Examples.Base
import Test.QuickCheck as QC

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

--TODO

fibExp2 :: Exp
fibExp2 =
  ELam
    "fib_n"
    TNat
    ( ERec
        (ETuple EZero (ESucc EZero))
        ( ELam
            "fib_t"
            (TProd TNat TNat)
            ( ETuple
                (ESnd (EVar "fib_t"))
                ( EApp
                    (EApp addExp (EFst (EVar "fib_t")))
                    (ESnd (EVar "fib_t"))
                )
            )
        )
        (EVar "fib_n")
    )

fibExp :: Exp
fibExp =
  ELam
    "fib_m"
    TNat
    (EFst (EApp fibExp2 (EVar "fib_m")))

-- | check that both versions agree
fibProp :: Property
fibProp = property $
  forAll (arbitrary `suchThat` (<= 10)) $ \n ->
    toNat (evalStar $ EApp fibExp (fromNat n)) === Just (fibHs n)

fibProg :: Program
fibProg = Program "fib" fibTy fibExp fibProp
