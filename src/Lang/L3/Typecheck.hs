{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lang.L3.Typecheck where

import Lang.L3.Syntax
import Test.QuickCheck

newtype TcTyExp = TcTyExp {tcgetExp :: Exp}
  deriving (Eq, Show)

newtype TC a = TC {runTC :: Either TCError a}
  deriving (Eq, Show, Functor, Applicative, Monad)

type TCError = String

tcfail :: TCError -> TC a
tcfail msg = TC (Left msg)

tcisSuccess :: TC a -> Bool
tcisSuccess (TC (Left _)) = False
tcisSuccess (TC (Right _)) = True

instance Arbitrary TcTyExp where
  arbitrary = TcTyExp <$> (arbitrary `suchThat` \e -> tcisSuccess $ tcinfer e)

--TC Check
tccheck :: Exp -> Ty -> TC ()
tccheck EZero TNat = return ()
tccheck (ESucc e) TNat =
  do
    _ <- tccheck e TNat
    return ()
tccheck ETrue TBool = return ()
tccheck EFalse TBool = return ()
tccheck (EAdd e1 e2) TNat =
  do
    _ <- tccheck e1 TNat
    _ <- tccheck e2 TNat
    return ()
tccheck (EMul e1 e2) TNat =
  do
    _ <- tccheck e1 TNat
    _ <- tccheck e2 TNat
    return ()
tccheck (EIf e1 e2 e3) ty =
  do
    _ <- tccheck e1 TBool
    _ <- tccheck e2 ty
    _ <- tccheck e3 ty
    return ()
tccheck e ty = tcfail ("check: " ++ show e ++ " is not an expression of type " ++ show ty ++ "!")

--TC infer
tcinfer :: Exp -> TC Ty
tcinfer EZero = return TNat
tcinfer (ESucc e) =
  do
    _ <- tccheck e TNat
    return TNat
tcinfer ETrue = return TBool
tcinfer EFalse = return TBool
tcinfer (EAdd e1 e2) =
  do
    _ <- tccheck e1 TNat
    _ <- tccheck e2 TNat
    return TNat
tcinfer (EMul e1 e2) =
  do
    _ <- tccheck e1 TNat
    _ <- tccheck e2 TNat
    return TNat
tcinfer (EIf e1 e2 e3) =
  do
    _ <- tccheck e1 TBool
    tcin2 <- tcinfer e2
    tcin3 <- tcinfer e3
    if tcin2 == tcin3
      then return tcin2
      else
        tcfail
          ( "infer: " ++ "EIf has different type in last two expression:\n" ++ show e2 ++ "has type of" ++ show tcin2 ++ "\n"
              ++ show e3
              ++ "has type of"
              ++ show tcin3
              ++ "\n"
          )
