-- | This is the interpreter for L1.


module Lang.L1.Eval where

import Common.Types
import Lang.L1.Syntax

eval :: Exp -> Nat
eval (ENat n) = n
eval (EAdd e1 e2) = eval e1 + eval e2
eval (EMul e1 e2) = eval e1 * eval e2


opt :: Exp -> Exp
opt (ENat n) = ENat n

opt (EAdd (ENat e1) (ENat e2)) = ENat (e1+e2)

opt (EMul _ (ENat 0)) = ENat 0
opt (EMul e1 (ENat(Succ e2))) = EAdd (opt e1) (opt (EMul e1 (ENat e2)))

opt (EMul (ENat 0) _) = ENat 0
opt (EMul (ENat(Succ e1)) e2 ) = EAdd (opt (EMul (ENat e1) e2)) (opt e2)

opt (EAdd e1 e2) = EAdd (opt e1) (opt e2)

opt (EMul e1 e2) = opt (EMul (opt e1) (opt e2))


-- multiplication by repeated addition

optMul :: Nat -> Exp -> Exp
optMul Zero _ = ENat 0
optMul (Succ n) e2 = EAdd e2 (optMul n e2)

-- partially evaluate multiplication to addition

opt2 :: Exp -> Exp
opt2 e@(ENat _) = e
opt2 (EAdd e1 e2) = EAdd (opt2 e1) (opt2 e2)
opt2 (EMul e1 e2) = optMul (eval e1) (opt2 e2)
