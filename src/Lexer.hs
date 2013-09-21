-- | The Lexer
module Lexer where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (GenParser)
import qualified Text.Parsec.Token as Tok

import Text.Parsec.Language (javaStyle)
import Model
import Keywords

type Parser a = GenParser Char Classes a
-- ----------------------------------------------------- [ Define Token Parser ]

lexer = Tok.makeTokenParser style
    where
      style = javaStyle {Tok.reservedOpNames = hUmlOps,
                            Tok.reservedNames = hUmlKeywords,
                            Tok.commentLine = "//"}

-- ------------------------------------------------------ [ Define Lexer Rules ]

-- | Do the lexing
runLex :: Parser a -> Parser a
runLex p = do
  Tok.whiteSpace lexer
  res <- p
  eof
  return res

-- | Core Lexer Operation
lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

colon :: Parser ()
colon = void (Tok.colon lexer)

-- | Lex these bad boys -> ','
comma :: Parser ()
comma = void (Tok.comma lexer)

-- | Lex these bad boys -> '[]'
brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

-- | Lex these base boys -> '()'
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- | Lex these bad boys -> '{}
braces :: Parser a -> Parser a
braces = Tok.braces lexer

-- | Lex keywords
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- | Lex operators
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- | Lex identifiers
identifier :: Parser String
identifier = Tok.identifier lexer

-- | Lex strings
stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

-- --------------------------------------------------------------------- [ EOF ]
