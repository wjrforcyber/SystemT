module Lang.L2.Typecheck where

import Data.Maybe
import Lang.L2.Syntax
import Test.QuickCheck

newtype TyExp = TyExp {getExp :: Exp}
  deriving (Eq, Show)

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
