module Main where

import System.Environment
import System.IO()
import qualified Data.Text as T

import Structures
import Codegen (codegenStatementNewline, boilerplate)
import Lexer (lexExpr, getAtoms)

lexString :: String -> Statement
lexString s = case (getAtoms s) of
                (Left _) -> NullStatement
                (Right x) -> lexExpr x

--parse single expression
emitOneC :: String -> String
emitOneC s = case (getAtoms s) of
               (Left _) -> ""
               (Right x) -> (codegenStatementNewline $ lexExpr $ x)

emitC :: String -> String
emitC x = boilerplate (unlines (map emitOneC (lines x)))

inputExtension :: T.Text
inputExtension = T.pack $ ".gigi"

outputExtension :: T.Text
outputExtension = T.pack $ ".c"

main :: IO ()
main = do
  args <- getArgs

  let inputFile = args !! 0
  let outputFile = T.unpack $ (T.replace inputExtension outputExtension) $ T.pack inputFile 
  
  srcCode <- (readFile inputFile)
  
  writeFile outputFile (emitC srcCode)
