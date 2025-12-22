module Structures where

data Expr
  = Atom String
  | List [Expr]
  deriving (Eq,Show)

getAtomValue :: Expr -> Maybe String
getAtomValue (Atom s) = Just s
getAtomValue (List _) = Nothing

getAtomValuePartial :: Expr -> String
getAtomValuePartial (Atom s) = s
getAtomValuePartial (List _) = ""

firstOfExpr :: Expr -> Maybe String
firstOfExpr (Atom _) = Nothing
firstOfExpr (List x) = getAtomValue (head x)

data CompileError
  = SyntaxError String
  deriving (Show)

data Statement
  = Assignment String Statement Statement
  | Defun String Statement Statement
  | Application Statement
  | Add Statement Statement
  | Subtract Statement Statement
  | Multiply Statement Statement
  | Divide Statement Statement
  | Power Statement Statement
  | Literal String
  | TypeDeclaration [Statement]
  | NullStatement
  | Error CompileError
  deriving (Show)

getLiteralValue :: Statement -> String
getLiteralValue (Literal x) = x
getLiteralValue _ = ""

extractType :: Statement -> [String]
extractType x = case x of
                  (TypeDeclaration xs) -> map (getLiteralValue) xs
                  _ -> ["auto"]

newline :: Char
newline = '\n'

addNewline :: String -> String
addNewline xs = (xs ++ [newline])

addSpace :: String -> String
addSpace x = x ++ " "

extractTypeAddSpace :: Statement -> [String]
extractTypeAddSpace x = map addSpace (extractType x)

errorType :: CompileError -> String
errorType (SyntaxError _) = "SyntaxError"

getErrorValue :: CompileError -> String
getErrorValue (SyntaxError x) = x

sayCompileError :: CompileError -> String
sayCompileError (SyntaxError x) =  "Syntax Error: " ++ x
