import Lang.L6.Eval.EEval
import Lang.L6.Syntax.Extrinsic

predecessor :: Exp -> Exp
predecessor EZero = error "EZero is not abiliable for predecessor"
predecessor (ESucc e) = eval e
predecessor (EIf e1 e2 e3) =
  do
    case eval e1 of
      ETrue -> predecessor (eval e2)
      EFalse -> predecessor (eval e3)
      _ -> error (show e1 ++ "value is not a Bool type")
predecessor (EFst e) =
  do
    case eval e of
      ETuple e1 _ -> predecessor (eval e1)
      _ -> error (show e ++ "cannot be apply to EFst")
predecessor (ESnd e) =
  do
    case eval e of
      ETuple _ e2 -> predecessor (eval e2)
      _ -> error (show e ++ "cannot be apply to ESnd")
-- predecessor (ELam name ty e) =
-- predecessor (EApp e1 e2) =
predecessor (ERec e1 e2 e3) =
  do
    case eval e3 of
      EZero -> predecessor e1
      ESucc e4 -> predecessor (EApp e2 (ERec e1 e2 e4))
      _ -> error "Couldn't predecessor"
predecessor _ = error "Expression is not aviliable for predecessor"
