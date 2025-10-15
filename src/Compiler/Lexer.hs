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

data FunctionPair = FunctionPair Expr [Expr] deriving (Eq, Show)

makeStatement :: Either ParseError [Expr] -> Maybe FunctionPair
makeStatement (Left _) = Nothing
makeStatement (Right []) = Nothing
makeStatement (Right (x:xs)) = Just (Statement x xs)

getAtomValue :: Expr -> Maybe String
getAtomValue (Atom s) = Just s
getAtomValue (List _) = Nothing

data Statement
  = Assignment String Expr
  | Defun String Expr
  | Application Expr 
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Power Expr Expr

convertExpr :: FunctionPair -> Maybe Statement

convertExpr (FunctionPair "=" [x1, x2]) = Just (Assignment (getAtomValue x1) x2)
convertExpr (FunctionPair "defun" [x1, x2]) = Just (Defun (getAtomValue x1) x2)

convertExpr (FunctionPair "$" [x1]) = Just (Application x1)

convertExpr (FunctionPair "+" [x1, x2]) = Just (Add x1 x2)
convertExpr (FunctionPair "-" [x1, x2]) = Just (Subtract x1 x2)
convertExpr (FunctionPair "*" [x1, x2]) = Just (Multiply x1 x2)
convertExpr (FunctionPair "/" [x1, x2]) = Just (Divide x1 x2)
convertExpr (FunctionPair "^" [x1, x2]) = Just (Power x1 x2)

convertExpr _ = Nothing 

