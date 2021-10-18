-- | Evaluator for L2

module Lang.L2.Eval where

import Lang.L2.Syntax

eval :: Exp -> Maybe Val
eval (ENat n) = Just (VNat n)
eval (EBool n) = Just (VBool n)


eval (EAdd e1 e2) =
    do VNat n <- eval(e1)
       VNat m <- eval(e2)
       return $ VNat (n + m)

eval (EMul e1 e2) =
    do VNat n <- eval(e1)
       VNat m <- eval(e2)
       return $ VNat (n * m)


eval (EIf e1 e2 e3) =
    do VBool n1 <- eval e1
       VNat n2 <- eval e2
       VNat n3 <- eval e3

       if n1 == True
           then return (VNat n2)
           else return (VNat n3)



eval (EIf e1 e2 e3) =
    do VBool n1 <- eval e1
       VBool n2 <- eval e2
       VBool n3 <- eval e3

       if n1 == True
           then return (VBool n2)
           else return (VBool n3)
