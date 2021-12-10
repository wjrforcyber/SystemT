module Lang.L2.Typecheck where

import Data.Maybe
import Lang.L2.Syntax
import Test.QuickCheck

newtype TyExp = TyExp {getExp :: Exp}
  deriving (Eq, Show)

newtype TcTyExp = TcTyExp {tcgetExp :: Exp}
  deriving (Eq, Show)

newtype TC a = TC {runTC :: Either TCError a}
  deriving (Eq, Show)

type TCError = String

instance Functor TC where
  fmap _ (TC (Left x)) = TC (Left x)
  fmap f (TC (Right y)) = TC (Right (f y))

instance Applicative TC where
  pure = TC . Right
  (TC (Left x)) <*> _ = TC (Left x)
  (TC (Right f)) <*> y = fmap f y

instance Monad TC where
  TC (Left x) >>= _ = TC (Left x)
  (TC (Right x)) >>= f = f x

tcfail :: TCError -> TC a
tcfail msg = TC (Left msg)

tcisSuccess :: TC a -> Bool
tcisSuccess (TC (Left _)) = False
tcisSuccess (TC (Right _)) = True

-- | The 'check' function takes an expression and a type and checks if the expression satisfies the type.
check :: Exp -> Ty -> Bool
check (ENat _) TNat = True
check (EBool _) TBool = True
check (EAdd e1 e2) TNat = check e1 TNat && check e2 TNat
check (EMul e1 e2) TNat = check e1 TNat && check e2 TNat
check (EIf e1 e2 e3) ty = check e1 TBool && (check e2 ty && check e3 ty)
check _ _ = False

-- | The 'infer' function takes an expression and tries to infer a correct type for it.
infer :: Exp -> Maybe Ty
infer (ENat _) =
  return TNat
infer (EBool _) =
  return TBool
infer (EAdd e1 e2) =
  do
    TNat <- infer e1
    TNat <- infer e2
    return TNat
infer (EMul e1 e2) =
  do
    TNat <- infer e1
    TNat <- infer e2
    return TNat
infer (EIf e1 e2 e3) =
  do
    TBool <- infer e1
    ty2 <- infer e2
    ty3 <- infer e3
    if ty2 == ty3
      then return ty2
      else Nothing

instance Arbitrary TyExp where
  arbitrary = TyExp <$> (arbitrary `suchThat` \e -> isJust $ infer e)

instance Arbitrary TcTyExp where
  arbitrary = TcTyExp <$> (arbitrary `suchThat` \e -> tcisSuccess $ tcinfer e)

--TC Check
tccheck :: Exp -> Ty -> TC ()
tccheck (ENat _) TNat = return ()
tccheck (EBool _) TBool = return ()
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
tccheck e ty = tcfail ("check: " ++ show e ++ "is not a type of" ++ show ty ++ "!\n")

--TC infer
tcinfer :: Exp -> TC Ty
tcinfer (ENat _) = return TNat
tcinfer (EBool _) = return TBool
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
