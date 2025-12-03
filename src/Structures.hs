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
  = Assignment String Statement
  | Defun String Statement
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

errorType :: CompileError -> String
errorType (SyntaxError _) = "SyntaxError"

getErrorValue :: CompileError -> String
getErrorValue (SyntaxError x) = x

sayCompileError :: CompileError -> String
sayCompileError (SyntaxError x) =  "Syntax Error: " ++ x
