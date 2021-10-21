module Lang.L2.Typecheck where

import Data.Maybe
import Lang.L2.Syntax
import Test.QuickCheck

newtype TyExp = TyExp {getExp :: Exp}
  deriving (Eq, Show)

newtype TC a = TC {runTC :: Either TCError a}
  deriving (Eq, Show)

type TCError = String

instance Functor TC where
  -- fmap:: (a -> b) -> TC a -> TC b
  fmap _ (TC (Left x)) = TC (Left x)
  fmap f (TC (Right y)) = TC (Right (f y))

instance Applicative TC where
  pure = TC . Right
  (TC (Left x)) <*> _ = TC (Left x)
  (TC (Right f)) <*> y = fmap f y

instance Monad TC where
  return x = TC (Right x)
  TC (Left x) >>= _ = TC (Left x)
  (TC (Right x)) >>= f = f x

-- fail _ = TC (Left x)

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

--TC Check
tccheck :: Exp -> Ty -> TC ()
tccheck (ENat _) TNat = return ()
tccheck (EBool _) TBool = return ()
tccheck (EAdd e1 e2) TNat =
  do
    tc1 <- tccheck e1 TNat
    tc2 <- tccheck e2 TNat
    if tc1 == () && tc2 == ()
      then return ()
      else TC (Left "EAdd has wrong expression")
tccheck (EMul e1 e2) TNat =
  do
    tc1 <- tccheck e1 TNat
    tc2 <- tccheck e2 TNat
    if tc1 == () && tc2 == ()
      then return ()
      else TC (Left "EMul has wrong expression")
tccheck (EIf e1 e2 e3) ty =
  do
    tc1 <- tccheck e1 TBool
    tc2 <- tccheck e2 ty
    tc3 <- tccheck e3 ty
    if tc1 == () && tc2 == () && tc3 == ()
      then return ()
      else TC (Left "EMul has wrong expression")
tccheck _ _ = TC (Left "Pattern not matching the defined expression!")

--TC infer
tcinfer :: Exp -> TC Ty
tcinfer (ENat _) = return TNat
tcinfer (EBool _) = return TBool
tcinfer (EAdd e1 e2) =
  do
    tcin1 <- tcinfer e1
    tcin2 <- tcinfer e2
    if tcin1 == TNat && tcin2 == TNat
      then return TNat
      else TC (Left "infer:EAdd has wrong expression")
tcinfer (EMul e1 e2) =
  do
    tcin1 <- tcinfer e1
    tcin2 <- tcinfer e2
    if tcin1 == TNat && tcin2 == TNat
      then return TNat
      else TC (Left "infer:EAdd has wrong expression")
tcinfer (EIf e1 e2 e3) =
  do
    tcin1 <- tcinfer e1
    tcin2 <- tcinfer e2
    tcin3 <- tcinfer e3
    if tcin1 == TBool && (tcin2 == tcin3)
      then return tcin2
      else TC (Left "infer:EIf has wrong expression")
