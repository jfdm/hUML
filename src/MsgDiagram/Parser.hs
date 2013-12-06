module MsgDiagram.Parser where

import Text.Parsec

import Data.Maybe
import Data.List

import MsgDiagram.Lexer
import MsgDiagram.Model

-- | Wrapper function.
parseMsgDiagram :: String -> Either ParseError MsgDiagram
parseMsgDiagram input = runParser (runLex doParse) 1 "" input
                        
doParse :: Parser MsgDiagram
doParse = do steps <- many parseStep
             eof
             return $ steps
          <?> "Parse Protocol"

parseStep :: Parser MsgDiagramStep
parseStep = do from <- identifier
               reservedOp "->"
               to <- identifier
               colon
               msgs <- commaSep1 parseMSG
               no <- getState
               updateState (+1)
               return $ MsgDiagramStep no from to msgs
            <?> "Protocol Step"

parseMSG :: Parser Message
parseMSG = do stringLiteral
      <?> "Message"



-- --------------------------------------------------------------------- [ EOF ]

