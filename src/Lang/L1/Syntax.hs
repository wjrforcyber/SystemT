-- | This is the Syntax of L1.
module Lang.L1.Syntax where

import Common.Types
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String
import qualified Text.Parsec.Token as P

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
