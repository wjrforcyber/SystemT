{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lang.L4.Typecheck where

import Lang.L4.Syntax
import Test.QuickCheck

newtype TcTyExp = TcTyExp {tcgetExp :: Exp}
  deriving (Eq, Show)

newtype TC a = TC {runTC :: Either TCError a}
  deriving (Eq, Show, Functor, Applicative, Monad)

instance MonadFail TC where
  fail = tcfail

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
tccheck EUnit TUnit = return ()
tccheck (ETuple e1 e2) (TProd ty1 ty2) =
  do
    _ <- tccheck e1 ty1
    _ <- tccheck e2 ty2
    return ()
tccheck (EFst e) ty =
  do
    TProd ty1 _ <- tcinfer e
    if ty1 == ty
      then return ()
      else tcfail ("check: the type of " ++ show e ++ "is " ++ show ty1 ++ ", not " ++ show ty)
tccheck (ESnd e) ty =
  do
    TProd _ ty2 <- tcinfer e
    if ty2 == ty
      then return ()
      else tcfail ("check: the type of " ++ show e ++ "is " ++ show ty2 ++ ", not " ++ show ty)
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
tcinfer EUnit = return TUnit
tcinfer (ETuple e1 e2) =
  do
    ty1 <- tcinfer e1
    ty2 <- tcinfer e2
    return (TProd ty1 ty2)
tcinfer (EFst e) =
  do
    TProd ty1 _ <- tcinfer e
    return ty1
tcinfer (ESnd e) =
  do
    TProd _ ty2 <- tcinfer e
    return ty2
