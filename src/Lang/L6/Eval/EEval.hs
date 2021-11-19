-- | eval envuator for Extrinsic L6
module Lang.L6.Eval.EEval where

import Lang.L6.Syntax.Extrinsic (Exp (..), Name (..))

data Env
  = Emp
  | Snoc Env (Name, Exp)

newtype Eval a = Eval {runEval :: Env -> Maybe a}

instance Functor Eval where
  fmap f (Eval g) = Eval $ \ctx -> fmap f (g ctx)

instance Applicative Eval where
  pure x = Eval $ \_ -> Just x
  Eval f <*> Eval x = Eval $ \ctx -> do
    f' <- f ctx
    x' <- x ctx
    return (f' x')

instance Monad Eval where
  Eval x >>= f = Eval $ \ctx -> do
    x' <- x ctx
    runEval (f x') ctx

-- lookupEnv :: Name -> Env -> Maybe Val
-- lookupEnv _ Emp = Nothing
-- lookupEnv x (Snoc env (y, v))
--   | x == y = Just v
--   | otherwise = lookupEnv x env

-- extendEnv :: Env -> Name -> Val -> Env
-- extendEnv env x v = Snoc env (x, v)

instance MonadFail Eval where
  fail _ = Eval $ const Nothing

readEnv :: Eval Env
readEnv = Eval pure

-- natToExp :: Nat -> Exp
-- natToExp Zero = EZero
-- natToExp (Succ n) = ESucc(natToExp n)

-- eval :: Exp -> Eval Val
-- eval EZero = return $ VSuccN 0
-- eval (ESucc e) =
--   do
--     VSuccN n <- eval e
--     return $ VSuccN (1 + n)
-- eval ETrue = return VTrue
-- eval EFalse = return VFalse
-- eval (EAdd e1 e2) =
--   do
--     VSuccN n <- eval e1
--     VSuccN m <- eval e2
--     return $ VSuccN (n + m)
-- eval (EMul e1 e2) =
--   do
--     VSuccN n <- eval e1
--     VSuccN m <- eval e2
--     return $ VSuccN (n * m)
-- eval (EIf e1 e2 e3) =
--   do
--     b1 <- eval e1
--     case b1 of
--       VTrue -> eval e2
--       VFalse -> eval e3
--       _ -> fail (show e1 ++ "has a type of" ++ show b1)

-- eval EUnit = return VUnit
-- eval (ETuple e1 e2) =
--   do
--     n <- eval e1
--     m <- eval e2
--     return $ VTuple n m
-- eval (EFst e) =
--   do
--     VTuple v1 _ <- eval e
--     return v1
-- eval (ESnd e) =
--   do
--     VTuple _ v2 <- eval e
--     return v2
-- eval (EVar name) =
--   do
--     env <- readEnv
--     case lookupEnv name env of
--       Just v -> return v
--       Nothing -> fail "unbound variable"
-- eval (ELam name ty e) =
--   return $ VLam name ty e
-- eval (EApp e1 e2) =
--   do
--     VLam name _ e <- eval e1
--     eval $ subst name e2 e

-- eval (ERec e1 e2 e3) =
--   do
--     VSuccN n <- eval e3
--     case n of
--       Zero -> eval e1
--       Succ e4 -> eval (EApp e2 (ERec e1 e2 (natToExp e4)))

subst :: Name -> Exp -> Exp -> Exp
subst x e (EVar y)
  | x == y = e
  | otherwise = EVar y
subst x e (ELam y ty e')
  | x == y = ELam y ty e'
  | otherwise = ELam y ty (subst x e e')
subst x e (EApp e1 e2) = EApp (subst x e e1) (subst x e e2)
subst x e (ETuple e1 e2) = ETuple (subst x e e1) (subst x e e2)
subst x e (EFst e') = EFst (subst x e e')
subst x e (ESnd e') = ESnd (subst x e e')
subst x e (EIf e1 e2 e3) = EIf (subst x e e1) (subst x e e2) (subst x e e3)
subst x e (ESucc e') = ESucc (subst x e e')
subst x e (ERec e1 e2 e3) = ERec (subst x e e1) (subst x e e2) (subst x e e3)
subst _ _ e = e

evalStep :: Exp -> Maybe Exp
-- computation rules
-- boolean
evalStep (EIf ETrue e2 _) =
  pure e2
evalStep (EIf EFalse _ e3) =
  pure e3
-- products
evalStep (EFst (ETuple e1 _)) =
  pure e1
evalStep (ESnd (ETuple _ e2)) =
  pure e2
-- functions
evalStep (EApp (ELam x _ e1) e2) =
  pure (subst x e2 e1)
-- naturals
evalStep (ERec e1 _ EZero) =
  pure e1
evalStep (ERec e1 e2 (ESucc e3)) =
  pure $ EApp e2 (ERec e1 e2 e3)
-- congruence rules
-- booleans
evalStep (EIf e1 e2 e3) =
  EIf <$> evalStep e1 <*> pure e2 <*> pure e3
-- products
evalStep (EFst e)
  | isVal e = Nothing
  | otherwise = EFst <$> evalStep e
evalStep (ESnd e)
  | isVal e = Nothing
  | otherwise = ESnd <$> evalStep e
evalStep (ETuple e1 e2)
  | isVal e1 && isVal e2 = Nothing
  | isVal e1 = ETuple <$> pure e1 <*> evalStep e2
  | otherwise = ETuple <$> evalStep e1 <*> pure e2
-- functions
--
-- naturals
evalStep (ESucc e)
  | isVal e = Nothing
  | otherwise = ESucc <$> evalStep e
evalStep (ERec e1 e2 e3) =
  ERec <$> pure e1 <*> pure e2 <*> evalStep e3
evalStep _ =
  Nothing

isVal :: Exp -> Bool
isVal EZero = True
isVal (ESucc e) = isVal e
isVal ETrue = True
isVal EFalse = True
isVal EUnit = True
isVal (ETuple e1 e2) = isVal e1 && isVal e2
isVal ELam {} = True
isVal _ = False

eval :: Exp -> Exp
eval e =
  maybe e eval (evalStep e)
