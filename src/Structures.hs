module Structures (Expr(Atom, List), Statement(Assignment, Defun, Application, Add, Subtract, Multiply, Divide, Power), CompileError(SyntaxError)) where

data Expr
  = Atom String
  | List [Expr]
  deriving (Eq,Show)

data Statement
  = Assignment String Expr
  | Defun String Expr
  | Application Expr
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Power Expr Expr
  deriving (Show)

data CompileError
  = SyntaxError String
  deriving (Show)
