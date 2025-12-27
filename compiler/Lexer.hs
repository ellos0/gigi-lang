module Lexer where

import Text.Parsec hiding (Error)
import Text.Parsec.String (Parser)

import Structures

symbol :: Parser Char
symbol = oneOf ("=+-/*$%_&!@#',")

eitherQuote :: Parser Char
eitherQuote = oneOf ([doubleQuote, singleQuote])

command :: Parser GigiVal
command = do
  _ <- char ':'
  x <- (many1 alphaNum)
  _ <- space
  xs <- (expression)
  return (Command x xs)

stringLiteral :: Parser GigiVal
stringLiteral = StringVal <$> (between (char doubleQuote) (char doubleQuote) (many1 (letter <|> digit <|> symbol <|> space)))

atom :: Parser GigiVal
atom = Atom <$> many1 (letter <|> digit <|> symbol)

list :: Parser GigiVal
list = List <$> (between (char '(') (char ')') (spaces *> sepBy expression spaces))

expression :: Parser GigiVal
expression = try list <|> atom <|> command <|> stringLiteral

getAtoms :: String -> Either ParseError GigiVal
getAtoms input = parse expression "" input

lexDefun :: [GigiVal] -> Statement
lexDefun xs = case (getAtomValue (xs !! 0)) of
                (Just name) -> (Defun name (lexExpr (xs !! 1)) (lexExpr (xs !! 2)))
                (Nothing) -> (Error (SyntaxError "Cannot name function using expression."))

lexAssignment :: [GigiVal] -> Statement
lexAssignment xs = case (getAtomValue (xs !! 0)) of
                     (Just name) -> (Assignment name (lexExpr (xs !! 1)) (lexExpr (xs !! 2)))
                     (Nothing) -> (Error (SyntaxError "Cannot name variable using expression."))

lexExpr :: GigiVal -> Statement
lexExpr (List []) = NullStatement

lexExpr (Command x xs) = CommandStatement x (lexExpr xs)
lexExpr (StringVal x) = StringLiteral x
lexExpr (Atom x) 
  | stringIsInteger x = IntLiteral x
  | otherwise = Literal x

lexExpr (List (x:xs)) = case (getAtomValue x) of
                          (Just "=") -> lexAssignment xs
                          (Just "defun") -> lexDefun xs
                          (Just "type") -> TypeDeclaration (map lexExpr xs)
                          (Just "+") -> Add (lexExpr (xs !! 0)) (lexExpr (xs !! 1))
                          (Just "-") -> Subtract (lexExpr (xs !! 0)) (lexExpr (xs !! 1))
                          (Just "*") -> Multiply (lexExpr (xs !! 0)) (lexExpr (xs !! 1))
                          (Just "/") -> Divide (lexExpr (xs !! 0)) (lexExpr (xs !! 1))
                          (Just "^") -> Power (lexExpr (xs !! 0)) (lexExpr (xs !! 1))
                          (Just _) -> Application (lexExpr x)
                          Nothing -> Multi (map lexExpr (x:xs))
