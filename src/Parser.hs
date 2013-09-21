-- | The parser.
module Parser (parseClassDiagram) where

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad
import Text.Parsec

import Data.Maybe
import Data.List

import Lexer
import Model
import Keywords


-- | Parses a Sif Spec file into the corresponding AST
parseClassDiagram :: String -> ClassDiagram
parseClassDiagram fname = case runParser (runLex parseCD) [] "" fname of
                            Left err -> error (show err)
                            Right ast -> ast

parseCD :: Parser ClassDiagram
parseCD = do cs <- many1 parseClass
             rs <- many1 parseRelation
             return $ ClassDiagram cs rs
          <?> "Class Diagram"

parseClass :: Parser Class
parseClass = do try parseClassC
             <|> parseClassS
             <?> "Class"

parseClassS :: Parser Class
parseClassS = do (mod, id) <- parseClassHead
                 let c = Class id mod Nothing Nothing
                 modifyState(c:)
                 return c
             <?> "Simple Class"

parseClassC :: Parser Class
parseClassC = do (mod, id) <- parseClassHead
                 (as, ms) <- braces parseClassBody
                 let c = Class id mod (Just as) (Just ms)
                 modifyState(c:)
                 return c
              <?> "Class with Body"

parseClassHead :: Parser (TyClass, String)
parseClassHead = do mod <- parseClassType
                    id <- identifier
                    return (mod, id)
                 <?> "Class"

parseClassType :: Parser TyClass
parseClassType = do try $ reserved hUmlModifierInterface
                    return InterfaceClass
             <|> do reserved hUmlModifierAbstract
                    reserved hUmlClass
                    return AbstractClass
             <|> do reserved hUmlClass
                    return NormalClass
             <?> "Class Type"

parseClassBody :: Parser (Attributes, Methods)
parseClassBody = do es <- many parseElement
                    let (as', ms') = unzip es
                    return (catMaybes as', catMaybes ms')
                 <?> "Class Body"

parseElement :: Parser (Maybe Attribute, Maybe Method)
parseElement = try (do attr <- parseAttribute
                       return (Just attr, Nothing))
                <|> do meth <- parseMethod
                       return (Nothing, Just meth)
                <?> "Element"

parseAttribute :: Parser Attribute
parseAttribute = do mod <- optionMaybe $ braces parseModifier
                    vis <- parseVisibility
                    id <- identifier
                    colon
                    t <- identifier
                    return $ Attribute id t mod vis
                    <?> "Attribute"

parseMethod :: Parser Method
parseMethod = do mod <- optionMaybe $ braces parseModifier
                 vis <- parseVisibility
                 id <- identifier
                 ps <- parens $ optionMaybe parseParams
                 colon
                 t <- identifier
                 return $ Method id t mod vis ps
                 <?> "Method"

parseModifier :: Parser Modifier
parseModifier = do try $ reservedOp hUmlModifierStatic
                   return Static
            <|> do reservedOp hUmlModifierAbstract
                   return Abstract
            <?> "Modifier"

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


parseParams :: Parser Params
parseParams = sepBy1 parseParam comma <?> "Params"

parseParam :: Parser Param
parseParam = do id <- identifier
                colon
                typ <- identifier
                return $ Param id typ
             <?> "Param"
                           
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

parseDesc :: Parser String
parseDesc = do reservedOp ":"
               stringLiteral
            <?> "Description"
{-

-- | Pattern Definition
-- parsePattern ::= parsePatternC | parsePatternS ;
parsePattern :: Parser ()
parsePattern = try parsePatternC <|> parsePatternS <?> "Patterns"


-- | Complex patterns that extend or import other patterns
-- parsePatternC ::= parsePatternS { parseProperty* };
parsePatternC :: Parser ()
parsePatternC = do (id, modifier, name) <- parsePatternHead
                   (extends, implements) <- braces parseProperties
                   let p = mkComplexPattern name id modifier extends implements
                   modifyState (p :)
               <?> "Complex Pattern"

-- | Simple patterns.
-- parsePatternS ::= parsePatternHead ;
parsePatternS :: Parser ()
parsePatternS = do (id, modifier, name) <- parsePatternHead
                   let p = mkSimplePattern name id modifier
                   modifyState (p :)
               <?> "Simple Pattern"


-- | Common Head of a Pattern
-- parsePatternHead ::= <id> '<-' parseModifier? Pattern(<name>);
parsePatternHead :: Parser (ID, Maybe Modifier, String)
parsePatternHead = do id <- identifier
                      reservedOp "<-"
                      modifier <- optionMaybe parseModifier
                      reserved "Pattern"
                      name <- parens stringLiteral                    
                      return (id, modifier, name)
                <?> "Pattern Head"
-}

getClass :: String -> Classes -> Maybe Class
getClass id [] = Nothing
getClass id cs = find (\x -> (classID x) == id) cs
-- -- --------------------------------------------------------------------- [ EOF ]
