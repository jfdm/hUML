module MsgDiagram.Lexer where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (GenParser)
import qualified Text.Parsec.Token as Tok

import Text.Parsec.Language (javaStyle)

type Parser a = GenParser Char Int a

-- ----------------------------------------------------- [ Define Token Parser ]

lexer = Tok.makeTokenParser style
    where
      style = javaStyle {Tok.reservedOpNames = ["->"],
                         Tok.reservedNames = [],
                         Tok.commentLine = "//"}

-- ------------------------------------------------------ [ Define Lexer Rules ]

-- | Do the lexing
runLex :: Parser a -> Parser a
runLex p = do Tok.whiteSpace lexer
              res <- p
              eof
              return res

-- | Core Lexer Operation
lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

-- | Lex these bad boys -> ','
comma :: Parser ()
comma = void (Tok.comma lexer)

-- | Lex these bad boys -> ':'
colon :: Parser ()
colon = void (Tok.colon lexer)

-- | Lex these bad boys -> ';'
semi :: Parser ()
semi = void (Tok.semi lexer)

-- | Lex these bad boys -> '[]'
brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

-- | Lex these base boys -> '()'
parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- | Lex these bad boys -> '{}'
braces :: Parser a -> Parser a
braces = Tok.braces lexer

-- | Lex these bad boys -> '||'
pipes :: Parser a -> Parser a
pipes = between (symbol "|") (symbol "|")

-- | Lex Symbols
symbol :: String -> Parser String
symbol = Tok.symbol lexer

-- | Lex Lists sep by comma
commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

-- | Lex Lists sep by comma
commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

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
