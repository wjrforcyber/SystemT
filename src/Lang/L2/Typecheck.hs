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

-- | The 'check' function takes an expression and a type and checks if the expression satisfies the type.
check :: Exp -> Ty -> Bool
check (ENat _) TNat = True
check (EBool _) TBool = True
check (EAdd e1 e2) TNat = check e1 TNat && check e2 TNat
check (EMul e1 e2) TNat = check e1 TNat && check e2 TNat
check (EIf e1 e2 e3) ty = check e1 TBool && (check e2 ty && check e3 ty)
check _ _ = False

-- check (EIf e1 e2 e3) TNat =  if check e1 TBool then if e1 == (EBool True) then check e2 TNat else check e3 TNat else False
-- check (EIf e1 e2 e3) TBool = if check e1 TBool then if e1 == (EBool True) then check e2 TBool else check e3 TBool else False

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

-- if infer e1 == Just TBool && infer e2 == infer e3 then infer e2 else Nothing
-- infer (EIf e1 e2 e3) = if check e1 TBool then if e1 == (EBool True) then infer e2 else infer e3 else Nothing

tccheck :: Exp -> Ty -> TC ()
tccheck (ENat _) TNat = TC (Right ())
tccheck (EBool _) TBool = TC (Right ())
tccheck (EAdd e1 e2) TNat = if tccheck e1 TNat == TC (Right ()) && tccheck e2 TNat == TC (Right ()) then TC (Right ()) else TC (Left "check: EAdd has wrong expression")
tccheck (EMul e1 e2) TNat = if tccheck e1 TNat == TC (Right ()) && tccheck e2 TNat == TC (Right ()) then TC (Right ()) else TC (Left "check: EMul has wrong expression")
tccheck (EIf e1 e2 e3) ty =
  if tccheck e1 TBool == TC (Right ())
    then
      if tccheck e2 ty == tccheck e3 ty
        then TC (Right ())
        else TC (Left "check: EIf has wrong expression in last two expression")
    else TC (Left "check: First expression in EIf is not available")
tccheck _ _ = TC (Left "Pattern not matching the defined expression!")

tcinfer :: Exp -> TC Ty
tcinfer (ENat _) = TC (Right TNat)
tcinfer (EBool _) = TC (Right TBool)
tcinfer (EAdd e1 e2) = if tcinfer e1 == TC (Right TNat) && tcinfer e2 == TC (Right TNat) then TC (Right TNat) else TC (Left "infer:EAdd has wrong expression")
tcinfer (EMul e1 e2) = if tcinfer e1 == TC (Right TNat) && tcinfer e2 == TC (Right TNat) then TC (Right TNat) else TC (Left "infer:EAdd has wrong expression")
tcinfer (EIf e1 e2 e3) =
  if tcinfer e1 == TC (Right TBool)
    then
      if tcinfer e2 == tcinfer e3
        then tcinfer e2
        else TC (Left "infer: EIf has wrong expression in last two expression")
    else TC (Left "infer: First expression in EIf is not available")
