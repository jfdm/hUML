-- | The Lexer.
--
-- Here we define the parsing functions that operate over keywords,
-- operators, and symbols within the language.
module ClassDiagram.Lexer where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (GenParser)
import qualified Text.Parsec.Token as Tok

import Text.Parsec.Language (javaStyle)

import ClassDiagram.Model
import ClassDiagram.Keywords

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

-- | Lex these bad boys -> ':'
colon :: Parser ()
colon = void (Tok.colon lexer)

-- | Lex these bad boys -> ','
comma :: Parser ()
comma = void (Tok.comma lexer)

-- | Lex these bad boys -> '[x]' and return x.
brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

-- | Lex these base boys -> '(x)' and return x
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- | Lex these bad boys -> '{x}' and return x
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
