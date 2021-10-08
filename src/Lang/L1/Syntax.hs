-- | This is the Syntax of L1.
module Lang.L1.Syntax where

import Common.Types
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Test.QuickCheck

data Exp
  = ENat Nat
  | EAdd Exp Exp
  | EMul Exp Exp
  deriving (Eq, Show)

-- Parser for Exp
-- See: http://ozark.hendrix.edu/~yorgey/360/f16/projects/ArithCompiler.html

lexer :: P.TokenParser u
lexer =
  P.makeTokenParser $
    emptyDef
      { P.opStart = oneOf "+*",
        P.opLetter = oneOf "+*",
        P.reservedOpNames = ["+", "*"]
      }

parens :: Parser a -> Parser a
parens = P.parens lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

nat :: Parser Nat
nat = fromInteger <$> P.natural lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parseAtom :: Parser Exp
parseAtom = ENat <$> nat <|> parens parseExp

parseExp :: Parser Exp
parseExp = buildExpressionParser table parseAtom <?> "expression"
  where
    table =
      [ [binary "*" EMul AssocLeft],
        [binary "+" EAdd AssocLeft]
      ]
    binary name fun assoc = Infix (reservedOp name >> return fun) assoc

parseLang :: Parser Exp
parseLang = whiteSpace *> parseExp <* eof

-- Prettyprinter for Exp
-- TODO: Print minimal number of parentheses

instance PP.Pretty Exp where
  pretty = prettyExp

prettyExp :: Exp -> PP.Doc ann
prettyExp (ENat n)     = PP.pretty n
prettyExp (EAdd e1 e2) = PP.parens (prettyExp e1 <+> PP.pretty "+" <+> prettyExp e2)
prettyExp (EMul e1 e2) = PP.parens (prettyExp e1 <+> PP.pretty "*" <+> prettyExp e2)


unsafeParse :: String -> Exp
unsafeParse s =
  case parse parseExp "" s of
    Left _ -> error "invalid expression"
    Right exp -> exp


hasEMul :: Exp -> Bool
hasEMul (ENat _) = False
hasEMul (EAdd e1 e2) = hasEMul e1 || hasEMul e2
hasEMul (EMul _ _) = True


instance Arbitrary Exp where
  arbitrary = sized arbExp

arbExp :: Int -> Gen Exp
arbExp 0 = ENat <$> arbitrary `suchThat` \n -> n < 30
arbExp n = do
  (Positive m) <- arbitrary
  let subExp = arbExp (n `div` (m + 1))
  oneof [subExp, EAdd <$> subExp <*> subExp, EMul <$> subExp <*> subExp]

