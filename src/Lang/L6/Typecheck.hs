module Lang.L6.Typecheck where

import Data.Either
import Lang.L6.Syntax.Extrinsic
import Test.QuickCheck

newtype TcTyExp = TcTyExp {tcgetExp :: Exp}
  deriving (Eq, Show)

newtype TC a = TC {runTC :: Ctx -> Either TCError a}

instance Functor TC where
  --fmap :: (a -> b) -> f a -> f b, f is functor, which will be repleced by TC (what we defined):
  --fmap :: (a -> b) -> TC a -> TC b
  fmap f (TC g) = TC $ \ctx -> fmap f (g ctx)

instance Applicative TC where
  --pure :: a -> f a
  --(<*>) :: f (a -> b) -> f a -> f b
  --From LYAH:
  --instance Applicative ((->) r) where
  --  pure x = (\_ -> x)
  --  f <*> g = \x -> f x (g x)
  -- pure takes a value and creates a function that ignores its parameter and always returns that value.
  -- If we look at the type for pure, but specialized for the (->) r instance, it's pure :: a -> (r -> a).
  pure a = TC $ \_ -> pure a
  (TC f) <*> (TC g) = TC $ \ctx -> f ctx <*> g ctx

instance Monad TC where
  --(>>=) :: m a -> (a -> m b) -> m b
  -- For Maybe, it will be Just x >>= f = f x, in TC,
  --                          TC f>>= g = TC $ \ctx -> f ctx >>= \a -> runTC (g a) ctx
  (TC f) >>= g = TC $ \ctx -> f ctx >>= \a -> runTC (g a) ctx

instance MonadFail TC where
  fail = tcfail

type TCError = String

tcfail :: TCError -> TC a
tcfail msg = TC $ \_ -> Left msg

tcisSuccess :: TC a -> Bool
tcisSuccess (TC f) = isRight (f Emp)

tcCtx :: TC Ctx
tcCtx = TC pure

-- x : A ∈ Γ

tclookup :: Name -> TC Ty
tclookup name =
  do
    ctx <- tcCtx
    case lookupCtx name ctx of
      Nothing -> tcfail ("Unbound name: " ++ show name)
      Just ty -> return ty

tcextend :: Name -> Ty -> TC a -> TC a
tcextend name ty (TC f) = TC $ \ctx -> f (extendCtx name ty ctx)

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
tccheck (EVar name) ty =
  do
    ty' <- tclookup name
    if ty' == ty
      then return ()
      else tcfail ("check: the type of " ++ show name ++ "is " ++ show ty' ++ ", not " ++ show ty)
tccheck (ELam name ty1 e) (TFun ty2 ty3) =
  if ty1 == ty2
    then tcextend name ty1 $ tccheck e ty3
    else tcfail ("check: the type of " ++ show name ++ "is " ++ show ty1 ++ ", not " ++ show ty2)
tccheck (EApp e1 e2) ty =
  do
    TFun ty1 ty2 <- tcinfer e1
    _ <- tccheck e2 ty1
    if ty2 == ty
      then return ()
      else tcfail ("check: the type of " ++ show (EApp e1 e2) ++ "is " ++ show ty2 ++ ", not " ++ show ty)
tccheck (ERec e1 e2 e3) ty =
  do
    _ <- tccheck e1 ty
    _ <- tccheck e2 (TFun ty ty)
    _ <- tccheck e3 TNat
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
tcinfer (EVar name) = tclookup name
tcinfer (ELam name ty1 e) =
  do
    ty2 <- tcextend name ty1 $ tcinfer e
    return (TFun ty1 ty2)
tcinfer (EApp e1 e2) =
  do
    TFun ty1 ty2 <- tcinfer e1
    _ <- tccheck e2 ty1
    return ty2
tcinfer (ERec e1 e2 e3) =
  do
    ty1 <- tcinfer e1
    _ <- tccheck e2 (TFun ty1 ty1)
    _ <- tccheck e3 TNat
    return ty1
