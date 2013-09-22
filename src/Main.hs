-- | Entry point to the program.
module Main (main) where

import System.Environment (getArgs)
import System.IO

import ClassDiagram

-- | Get AST andn spit out a Dot representation.
main :: IO ()
main = do args <- getArgs 
          content <- readFile $ head args
          let ast = parseClassDiagram content
          print $ toDotClassDiagram ast
-- --------------------------------------------------------------------- [ EOF ]
