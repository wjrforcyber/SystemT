-- | Evaluator for L2

module Lang.L2.Eval where

import Lang.L2.Syntax

addJust :: Maybe Val-> Maybe Val ->Maybe Val
addJust (Just (VNat n)) (Just (VNat m)) = Just (VNat (n+m))
addJust _ _ = Nothing

mulJust :: Maybe Val-> Maybe Val ->Maybe Val
mulJust (Just (VNat n)) (Just (VNat m)) = Just (VNat (n*m))
mulJust _ _ = Nothing

typeEq :: Maybe Val -> Maybe Val -> Bool
typeEq (Just (VNat _)) (Just (VNat _)) = True
typeEq (Just (VBool _)) (Just (VBool _)) = True
typeEq _ _ = False


eval :: Exp -> Maybe Val
eval (ENat n) = Just (VNat n)
eval (EBool n) = Just (VBool n)


eval (EAdd e1 e2) = addJust (eval(e1)) (eval(e2))

eval (EMul e1 e2) = mulJust (eval(e1)) (eval(e2))

eval (EIf e1 e2 e3) = if typeEq (eval(e2)) (eval(e3)) && ((eval e1) == (Just (VBool True)) || (eval e1) == (Just (VBool False)))
                        then
                        if (eval e1) == Just (VBool True)
                            then eval(e1)
                            else eval(e2)
                        else Nothing
