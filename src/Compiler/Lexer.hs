module Compiler.Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)

data Expr
  = Atom String
  | List [Expr]
  deriving (Eq,Show)

atom :: Parser Expr
atom = Atom <$> many1 alphaNum

list :: Parser Expr
list = List <$> (between (char '(') (char ')') (spaces *> sepBy expression spaces))

expression :: Parser Expr
expression = try list <|> atom

getAtoms :: String -> Either ParseError [Expr]
getAtoms input = parse (spaces *> sepBy expression spaces <* eof) "" input

data OperatorPair = OperatorPair {op :: Expr, operands :: [Expr]} deriving (Eq, Show)

makeOperatorPair :: Either ParseError [Expr] -> Maybe OperatorPair
makeOperatorPair (Left _) = Nothing
makeOperatorPair (Right []) = Nothing
makeOperatorPair (Right (x:xs)) = Just (OperatorPair {op = x, operands = xs})
