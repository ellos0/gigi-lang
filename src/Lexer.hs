module Lexer where

import Text.Parsec
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

data FunctionPair = FunctionPair Expr [Expr] deriving (Eq, Show)

makeStatement :: Expr -> FunctionPair
makeStatement (List (x:xs)) = FunctionPair x xs

getAtomValue :: Expr -> Maybe String
getAtomValue (Atom s) = Just s
getAtomValue (List _) = Nothing

convertExpr :: FunctionPair -> Either CompileError Statement

convertExpr (FunctionPair (Atom "=") [x1, x2]) = case (getAtomValue x1) of
                                                 Just atom -> Right (Assignment atom x2)
                                                 Nothing -> Left (SyntaxError "Cannot name a variable with an expression")

convertExpr (FunctionPair (Atom "defun") [x1, x2]) = case (getAtomValue x1) of 
                                                     Just atom -> Right (Defun atom x2)
                                                     Nothing -> Left (SyntaxError "Cannot name a function with an expression")

convertExpr (FunctionPair (Atom "$") [x1]) = Right (Application x1)

convertExpr (FunctionPair (Atom "+") [x1, x2]) = Right (Add x1 x2)
convertExpr (FunctionPair (Atom "-") [x1, x2]) = Right (Subtract x1 x2)
convertExpr (FunctionPair (Atom "*") [x1, x2]) = Right (Multiply x1 x2)
convertExpr (FunctionPair (Atom "/") [x1, x2]) = Right (Divide x1 x2)
convertExpr (FunctionPair (Atom "^") [x1, x2]) = Right (Power x1 x2)

convertExpr _ = Left (SyntaxError "Invalid Statement")

sayCompileError :: Either CompileError Statement -> Maybe String
sayCompileError (Left (SyntaxError x)) = Just ("Syntax Error: " ++ x)
sayCompileError _ = Nothing

parseExpr :: Expr -> Either CompileError Statement
parseExpr = convertExpr . makeStatement 

parseExprs :: [Expr] -> [Either CompileError Statement]
parseExprs = map parseExpr 

parseString :: String -> Maybe [Either CompileError Statement]
parseString x = case getAtoms x of
    Left _ -> Nothing
    Right exprs -> Just (parseExprs exprs)
