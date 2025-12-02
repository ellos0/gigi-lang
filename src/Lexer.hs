module Lexer where

import Text.Parsec (many1, letter, digit, oneOf, between, char, spaces, sepBy, eof, ParseError, parse, try, (<|>))
import Text.Parsec.String (Parser)
import Structures

atom :: Parser Expr
atom = Atom <$> many1 (letter <|> digit <|> oneOf "=+-/*$^")

list :: Parser Expr
list = List <$> (between (char '(') (char ')') (spaces *> sepBy expression spaces))

expression :: Parser Expr
expression = try list <|> atom

getAtoms :: String -> Either ParseError [Expr]
getAtoms input = parse (spaces *> sepBy expression spaces <* eof) "" input

lexExpr :: Expr -> Statement
lexExpr (List []) = NullStatement
lexExpr (Atom x) = Literal x
lexExpr (List (x:xs)) = case (getAtomValuePartial x) of
                          "=" -> case (getAtomValue (xs !! 0)) of
                            (Just name) -> (Assignment name (lexExpr (xs !! 0)))
                            (Nothing) -> (Error (SyntaxError "Cannot name variable using expression."))
                          "defun" -> case (getAtomValue (xs !! 0)) of
                            (Just name) -> (Defun name (lexExpr (xs !! 0)))
                            (Nothing) -> (Error (SyntaxError "Cannot name function using expression."))
                          "$" -> Application (lexExpr (xs !! 0))
                          "+" -> Add (lexExpr (xs !! 0)) (lexExpr (xs !! 1))
                          "-" -> Subtract (lexExpr (xs !! 0)) (lexExpr (xs !! 1))
                          "*" -> Multiply (lexExpr (xs !! 0)) (lexExpr (xs !! 1))
                          "/" -> Divide (lexExpr (xs !! 0)) (lexExpr (xs !! 1))
                          "^" -> Power (lexExpr (xs !! 0)) (lexExpr (xs !! 1))
                          _ -> (Error (SyntaxError "Term not recongnized as a function, operator, or other known device."))                          
