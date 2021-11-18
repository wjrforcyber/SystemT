module Lang.L6.Examples.Base
  ( module Common.Types,
    module Lang.L6.Syntax.Extrinsic,
    Program (..),
    fromNat,
    toNat,
  )
where

import Common.Types
import Lang.L6.Syntax.Extrinsic
import qualified Test.QuickCheck as QC

data Program = Program
  { -- | Name of the program
    progName :: String,
    -- | Type of the program
    progTy :: Ty,
    -- | Body of the program
    progExp :: Exp,
    -- | Property satisfied by the program
    progProp :: QC.Property
  }

fromNat :: Nat -> Exp
fromNat Zero = EZero
fromNat (Succ n) = ESucc (fromNat n)

toNat :: Exp -> Maybe Nat
toNat EZero = Just Zero
toNat (ESucc e) = Succ <$> toNat e
toNat _ = Nothing
