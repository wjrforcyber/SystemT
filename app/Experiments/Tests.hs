-- | This is an old experiment.

-- module Experiments.Tests where

-- data Nat = Zero | Succ Nat deriving (Eq, Show)

-- -- Expression
-- data Exp = Var String          -- variable
--          | App Exp Exp         -- application
--          | Lam String Exp      -- abstraction
--          | I   Int             -- Int
--          | N   Nat             -- Nat
--          | Add Exp Exp         -- Add
--          | Mul Exp Exp         -- Multiply
--          | B   Bool            -- Bool
--          | And Exp Exp
--          | Or  Exp Exp
 
--          deriving (Eq, Show)


-- -- value definition
-- data Val = VI  Int                        -- Int 
--          | VN   Nat                        -- Nat
--          | VB   Bool          
--          | VLam String Exp [(String, Val)] -- abstraction
--          deriving (Eq, Show)

-- -- top level evaluation
-- eval :: Exp -> Maybe Val 
-- eval exp = ev [] exp 

-- -- Maybe
-- bind :: Maybe a -> (a -> Maybe b) -> Maybe b
-- bind Nothing  f = Nothing
-- bind (Just v) f = f v

-- -- inner level eval, enviroment, a list contain variable and value
-- ev :: [(String, Val)] -> Exp -> Maybe Val

-- -- variables
-- ev []           (Var x)        = Nothing
-- ev ((x',v):env) (Var x)        = if x == x' then Just v else ev env (Var x) --Search from left to right for the value of current variable

-- -- Nat Bool Int
-- ev env (N x)                   = Just (VN x)

-- ev env (B x)                   = Just (VB x)

-- ev env (I x)                   = Just (VI x)

-- -- abstraction
-- ev env (Lam x body)            = Just (VLam x body env)

-- -- Int operation
-- ev env (Add x y)               = bind (ev env x) $ \vx ->
--                                       bind (ev env y) $ \vy -> 
--                                         intPlus vx vy

-- ev env (Mul x y)               = bind (ev env x) $ \vx ->
--                                    bind (ev env y) $ \vy ->
--                                         intMul vx vy

-- -- Bool operation
-- ev env (And x y)               = bind (ev env x) $ \vx ->
--                                    bind (ev env y) $ \vy ->
--                                        boolAnd vx vy

-- ev env (Or x y)                = bind (ev env x) $ \vx ->
--                                    bind (ev env y) $ \vy ->
--                                        boolOr vx vy


-- -- application                                   
-- ev env (App f arg)             = bind (ev env f) $ \vf -> 
--                                    bind (ev env arg) $ \varg -> 
--                                      applyLam vf varg


-- -- Nat plus
-- natPlus :: Nat -> Nat -> Nat
-- natPlus Zero       m   =  m
-- natPlus (Succ(n))  m   =  Succ(natPlus n m)

-- -- Nat mul
-- natMul :: Nat -> Nat -> Nat
-- natMul  Zero     m = Zero
-- natMul (Succ n)  m = natPlus (natMul n m) m

-- --Int plus
-- intPlus :: Val -> Val -> Maybe Val
-- intPlus (VI x) (VI y) = Just (VI (x+y))

-- --Int mul
-- intMul :: Val -> Val -> Maybe Val
-- intMul (VI x) (VI y) = Just (VI (x*y))

-- -- Bool And
-- boolAnd :: Val -> Val -> Maybe Val
-- boolAnd (VB False)  _   = Just (VB False)
-- boolAnd _  (VB False)   = Just(VB False)
-- boolAnd _     _         = Just (VB True)

-- -- Bool Or
-- boolOr :: Val -> Val -> Maybe Val
-- boolOr (VB True) _ = Just (VB True)
-- boolOr _ (VB True) = Just (VB True)
-- boolOr _    _      = Just (VB False)


-- natType :: Nat -> Maybe Val
-- natType x  = Just (VN x) 


-- -- function application
-- applyLam :: Val -> Val -> Maybe Val
-- applyLam (VLam x body env) arg = ev ((x,arg):env) body



-- -- |> for application, reference: https://www.haskell.org/onlinereport/decls.html
-- infixl 1 |>
-- (|>) = App


-- test1 = natType(natMul (Succ(Succ(Succ(Zero)))) (Succ(Succ(Zero))))
-- test2 = (Lam "x"(Lam "y"(Or (Var "x") (Var "y")))) |> B False |> B False
-- test3 = And (B True) (B True)
-- test4 = Or (B False) (B False)
-- test5 = Add (I 9) (I 6)
-- test6 = (Lam "x"(Lam "y"(Add (Var "x") (Var "y")))) |> I 9 |> I 6



-- main = do 
--         --  TODO:
--         -- test1 Just make sure defination of Nat and Mul is working fine but there is no evaluation, 
--         -- how to change Nat->Nat->Nat to Val->Val->Maybe Val?
--          print $test1
        
--         -- These are fine :)
--          print $ eval test2
--          print $ eval test3
--          print $ eval test4
--          print $ eval test5
--          print $ eval test6



-- This is a new test
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

-- module Experiments.Tests where
module Main where

import Lang.L1.Eval
import Lang.L1.Syntax

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.Hspec as Hspec
import Control.Exception (evaluate)

import Test.Tasty.HUnit

-- import Data.List
-- import Data.Ord

import Test.QuickCheck.Gen

import Test.SmallCheck.Series as SCS
import Control.Monad.Identity

-- main = defaultMain tests
main = do
  -- hspecTestsTree <- Hspec.testSpec "checked by Hspec" hspecTests
  defaultMain tests
  


-- SmallCheck instance(Not good, TODO: a list? Or could sample like QC?)
instance (Monad m) => Serial m Exp where
     series = SCS.generate(\d->Zero:take (d-1) [Succ(Zero),Succ(Succ(Zero))])


-- QuickCheck instance
gExp :: Gen Exp
gExp = oneof [pure Zero, Succ <$> gExp]

instance Arbitrary Exp where
  arbitrary = gExp




tests :: TestTree
tests = testGroup "Tests" [properties,unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps,qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ 
    SC.testProperty "Evaluate Zero" $
    (eval (Zero :: Exp)) == 0
  , SC.testProperty "Succ" $
      \x -> (eval (x::Exp))+1 == ((eval (Succ(x))) :: Int)
  -- the following property does not hold
  , SC.testProperty "Addition" $
      \x y ->
        (eval (x :: Exp)) + (eval (y :: Exp)) == (eval (Add x y) :: Int)
  ]


qcProps = testGroup "(Checked by QucikCheck)"
  [ 
    QC.testProperty "Evaluate Zero" $
    (eval (Zero :: Exp)) == 0
  , QC.testProperty "Succ" $
      \x -> (eval (x::Exp))+1 == ((eval (Succ(x))) :: Int)
  -- the following property does not hold
  , QC.testProperty "Addition" $
      \x y ->
        (eval (x :: Exp)) + (eval (y :: Exp)) == (eval (Add x y) :: Int)
  ]


unitTests = testGroup "Unit tests"
  [
    testCase "Zero" $
    eval(Zero) @?= 0

    ,testCase "Succ" $
    (eval (Succ(Succ(Succ(Zero))))) @?= ((1+(eval (Succ(Succ(Zero))))) :: Int)

    ,testCase "Addition" $
    (eval (Succ(Succ(Succ(Zero))))) + (eval (Succ(Succ(Zero)))) @?= (eval (Succ(Succ(Succ(Succ(Succ(Zero)))))) :: Int)


  ]

  

-- hspecTests = do
--   describe "test1" $
--     it "0 == 0" $
--      (eval (Zero :: Exp)) == 0
--   describe "test2" $
--     it "Succ" $
--      \x -> (eval (x::Exp))+1 == ((eval (Succ(x))) :: Int)
--   describe "test3" $
--     it "Addition" $
--      \x y ->
--         (eval (x :: Exp)) + (eval (y :: Exp)) == (eval (Add x y) :: Int)

