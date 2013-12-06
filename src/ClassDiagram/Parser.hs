-- | The parser.
--
-- Here we define the parser for our Class Diagram language. The
-- parser does not replicate the eBNF grammar givin in the
-- specification exactly. This is due to optimisations and
-- choice/idiomatic programming. The productions rules associated with
-- each parse operation are given in the comments.
module ClassDiagram.Parser (parseClassDiagram) where

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad
import Text.Parsec

import Data.Maybe
import Data.List

import ClassDiagram.Lexer
import ClassDiagram.Model
import ClassDiagram.Keywords

-- | Wrapper function.
parseClassDiagram :: String -> Either ParseError ClassDiagram
parseClassDiagram input = runParser (runLex parseCD) [] "" input

-- | Do the parsing
-- @
--    parseCD ::= parseClass+ parseRelation+ ;
-- @
parseCD :: Parser ClassDiagram
parseCD = do cs <- many1 parseClass
             rs <- many1 parseRelation
             return $ ClassDiagram cs rs
         <?> "Class Diagram"

-- ----------------------------------------------------------------- [ Classes ]
-- | A Class.
-- @
--     parseClass ::= parseClassC | parseClassS ;
-- @
parseClass :: Parser Class
parseClass = try parseClassC <|> parseClassS <?> "Class"

-- | A class with no body
-- @
--     parseClassS ::= parseClassHead ;
-- @
parseClassS :: Parser Class
parseClassS = do (mod, id) <- parseClassHead
                 let c = Class id mod Nothing Nothing
                 modifyState(c:)
                 return c
             <?> "Simple Class"

-- | A class with a body
-- @
--     parseClassC ::= parseClassHead parseClassBody ;;
-- @
parseClassC :: Parser Class
parseClassC = do (mod, id) <- parseClassHead
                 (as, ms) <- braces parseClassBody
                 let c = Class id mod (Just as) (Just ms)
                 modifyState(c:)
                 return c
              <?> "Class with Body"

-- | A class definitions...
-- @
--     parseClassHead ::= parseClassType <id> ;
-- @
parseClassHead :: Parser (TyClass, String)
parseClassHead = do mod <- parseClassType
                    id <- identifier
                    return (mod, id)
                 <?> "Class"

-- | Types of class
-- @
--     parseClassType ::= ("interface" | "abstract"? "class") <id>";
-- @
parseClassType :: Parser TyClass
parseClassType = do try $ reserved hUmlModifierInterface
                    return InterfaceClass
             <|> do reserved hUmlModifierAbstract
                    reserved hUmlClass
                    return AbstractClass
             <|> do reserved hUmlClass
                    return NormalClass
             <?> "Class Type"

-- | Class bodies
-- @
--     parseClassBody ::= parseElement* ;
-- @
parseClassBody :: Parser (Attributes, Methods)
parseClassBody = do es <- many parseElement
                    let (as', ms') = unzip es
                    return (catMaybes as', catMaybes ms')
                 <?> "Class Body"

-- | Bodies are...
-- @
--     parseElement ::= parseAttribute | parseMethod ;
-- @
parseElement :: Parser (Maybe Attribute, Maybe Method)
parseElement = try (do attr <- parseAttribute
                       return (Just attr, Nothing))
                <|> do meth <- parseMethod
                       return (Nothing, Just meth)
                <?> "Element"

-- -------------------------------------------------------------- [ Attributes ]
-- | Parse attributes
-- @
--     parseAttribute ::= ('{' parseModifier '}')? parseVisibility
--                         <id> ':' <typeID> ;
-- @
parseAttribute :: Parser Attribute
parseAttribute = do mod <- optionMaybe $ braces parseModifier
                    vis <- parseVisibility
                    id <- identifier
                    colon
                    t <- identifier
                    return $ Attribute id t mod vis
                    <?> "Attribute"

-- ----------------------------------------------------------------- [ Methods ]
-- | Parse methods
-- @
--     parseMethod ::= ('{' parseModifier '}')? parseVisibility <id>
--                     '(' parseParams ')' ':' <typeID> ;
-- @
parseMethod :: Parser Method
parseMethod = do mod <- optionMaybe $ braces parseModifier
                 vis <- parseVisibility
                 id <- identifier
                 ps <- parens $ optionMaybe parseParams
                 colon
                 t <- identifier
                 return $ Method id t mod vis ps
                 <?> "Method"

-- ------------------------------------------------------------------ [ Params ]
-- | Parse parameters
-- @
--     parseParams ::= parseParam (',' parseParam)* ;
-- @

parseParams :: Parser Params
parseParams = sepBy1 parseParam comma <?> "Params"

-- | Parse a parameter
-- @
--     parseParam ::= <id> ':' <typeID> ;
-- @
parseParam :: Parser Param
parseParam = do id <- identifier
                colon
                typ <- identifier
                return $ Param id typ
             <?> "Param"
                      
-- --------------------------------------------------------------- [ Relations ]     
-- | Parse a relation
-- @
--     parseRelation ::= <id> parseRelationType <id> parseDesc? ;
-- @
parseRelation :: Parser Relation
parseRelation = do f <- identifier
                   rtype <- parseRelationType
                   t <- identifier
                   desc <- optionMaybe parseDesc
                   cs <- getState
                   let from = getClass f cs
                   if isNothing from
                   then fail "Class not defined"
                   else do let to = getClass t cs
                           if isNothing to
                           then fail "Class not defined"
                           else return $ Relation rtype (fromJust from) (fromJust to) desc
                <?> "Relation"

-- | Parse relation type
-- @
--     parseRelationType ::= '<|-' | 'o--' | '*--' | '-->' ;
-- @
parseRelationType :: Parser TyRelation
parseRelationType = do try $ reservedOp hUmlOperatorSpecialisation
                       return Specialisation
                <|> do reservedOp hUmlOperatorComposition
                       return Composition
                <|> do reservedOp hUmlOperatorAggregation   
                       return Aggregation
                <|> do reservedOp hUmlOperatorRealisation
                       return Realisation
                <|> do reservedOp hUmlOperatorAssociation
                       return Association
                <?> "Relation Type"

-- | Parse descriptions
-- @
--     parseDesc ::= ':' <description>
-- @
parseDesc :: Parser String
parseDesc = do reservedOp ":"
               stringLiteral
            <?> "Description"

-- -------------------------------------------------------------------- [ Misc ]
-- | Parse Modifiers
-- @
--     parseModifier ::= 'static' | 'abstract' ;
-- @
parseModifier :: Parser Modifier
parseModifier = do try $ reservedOp hUmlModifierStatic
                   return Static
            <|> do reservedOp hUmlModifierAbstract
                   return Abstract
            <?> "Modifier"

-- | Parse visibility
-- @
--     parseVisiblity ::= '-' | '+' | '#' | '~' ;
-- @
parseVisibility :: Parser Visibility
parseVisibility = do try $ reservedOp hUmlVisibilityPrivate
                     return Private
                  <|> do reservedOp hUmlVisibilityPublic
                         return Public
                  <|> do reservedOp hUmlVisibilityPackage
                         return Package
                  <|> do reservedOp hUmlVisibilityProtected
                         return Protected
                  <?> "Visibility"

-- ------------------------------------------------------------------- [ Utils ]

getClass :: String -> Classes -> Maybe Class
getClass id [] = Nothing
getClass id cs = find (\x -> classID x == id) cs

-- --------------------------------------------------------------------- [ EOF ]
