module Main where

import Structures
import Codegen (codegenStatementAddSemicolon)
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
emitC x = unlines (map emitOneC (lines x))

main :: IO ()
main = interact (emitC)
