module Main (main) where

import System.Environment (getArgs)
import System.IO

import Parser
import Model
import Transform.Dot

main :: IO ()
main = do args <- getArgs 
          content <- readFile $ head args
          let ast = parseClassDiagram content
          print $ toDotClassDiagram ast
