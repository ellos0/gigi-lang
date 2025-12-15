module Main where

import Structures
import Codegen (codegenStatementAddSemicolon, boilerplate)
import Lexer (lexExpr, getAtoms)

lexString :: String -> Statement
lexString s = case (getAtoms s) of
                (Left _) -> NullStatement
                (Right x) -> lexExpr x

--parse single expression
emitOneC :: String -> String
emitOneC s = case (getAtoms s) of
               (Left _) -> ""
               (Right x) -> (codegenStatementAddSemicolon $ lexExpr $ x)

emitC :: String -> String
emitC x = boilerplate (unlines (map emitOneC (lines x)))

main :: IO ()
main = interact (emitC)
