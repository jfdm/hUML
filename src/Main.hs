-- | 
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.IO
import System.FilePath
import System.Exit

import Text.Show.Pretty
import Text.PrettyPrint.Leijen as PP

import Text.Parsec.Error

import qualified ClassDiagram as CD
import qualified MsgDiagram as MD

-- | Do stuff
main :: IO ()
main = do
  args <- getArgs
  opts <- (if null args
           then withArgs ["--help"]
           else id) $ cmdArgs hUMLOptions
 
  case from opts of
    "class" -> doStuff opts CD.parseClassDiagram CD.transformClassDiagram
    "msd"   -> doStuff opts MD.parseMsgDiagram   MD.transformMsgDiagram
    otherwise -> do
         putStrLn "Input Format needs to be specified"
         exitWith (ExitFailure 1)

doStuff :: Show a => 
           HumlOptions                                  -- ^ Options
        -> (String -> Either ParseError a)              -- ^ Parsing Function
        -> (String -> a -> Either String (String, Doc)) -- ^ Transformation Function
        -> IO ()
doStuff opts parse transform = do
  contents <- readFile (file opts)
  case parse contents of
    Left err  -> do
      putStrLn $ file opts ++ " failed to parse"
      print err
      exitWith (ExitFailure 1)
    Right res -> do
      if model opts
         then do
           let fname = addExtension (file opts) ".model"
           writeFile fname (ppShow res)
           putStrLn $ "Model for: "
                      ++ (file opts)
                      ++ " has been written in "
                      ++ fname
         else putStrLn ""
      case transform (to opts) res of
        Left err -> do
                   print err
                   exitWith (ExitFailure 1)
        Right (ext, doc) -> if null (output opts)
                              then print doc
                              else do
                                let fname = addExtension (output opts) ext
                                handle <- openFile fname WriteMode
                                hPutDoc handle doc
                                hClose handle
                                putStrLn $ "File: "
                                        ++ fname
                                        ++ " has been written."
      exitSuccess

-- ----------------------------------------------------------------- [ Options ]

-- | Options the type checker takes.
data HumlOptions = HumlOptions {
      to     :: String,   -- ^ The output format
      from   :: String,   -- ^ The input format
      output :: FilePath, -- ^ Output filename.
      model  :: Bool,     -- ^ Print the resulting model
      file   :: FilePath  -- ^ The input file.
    } deriving (Show, Data, Typeable)

-- | Set default options
hUMLOptions :: HumlOptions
hUMLOptions = HumlOptions {
                to = def
                  &= typ "FORMAT"
                  &= help "Format to output",
                from = def
                  &= typ "FORMAT"
                  &= help "Input Format",
                output = def
                  &= typ "FILENAME"
                  &= help "The name of file to write the output too.",
                model = def
                  &= help "Save the Model",
                file = def
                  &= typFile
                  &= argPos 0
              }
              &= summary "hUML (C) Jan de Muijnck-Hughes 2013"
              &= program "hUML"
              &= details ["hUML is a tool to parse various UML diagrams into other formats."
                         ]
-- --------------------------------------------------------------------- [ EOF ]
