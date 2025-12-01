module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Either (lefts, rights)
import Structures

atom :: Parser Expr
atom = Atom <$> many1 (letter <|> digit <|> oneOf "=+-/*$^")

list :: Parser Expr
list = List <$> (between (char '(') (char ')') (spaces *> sepBy expression spaces))

expression :: Parser Expr
expression = try list <|> atom

getAtoms :: String -> Either ParseError [Expr]
getAtoms input = parse (spaces *> sepBy expression spaces <* eof) "" input

getAtomValue :: Expr -> Maybe String
getAtomValue (Atom s) = Just s
getAtomValue (List _) = Nothing

firstOfExpr :: Expr -> Maybe String
firstOfExpr (Atom _) = Nothing
firstOfExpr (List x) = getAtomValue (head x)

lexExpr :: Expr -> Either CompileError Statement 

sayCompileError :: CompileError -> String
sayCompileError (SyntaxError x) = ("Syntax Error: " ++ x)

throwCompileError :: CompileError -> ()
throwCompileError x = error (sayCompileError x)

throwErrors :: [Either CompileError Statement] -> [()]
throwErrors xs = map throwIf xs
               where
                 throwIf (Left x) = throwCompileError x
                 throwIf _ = ()
