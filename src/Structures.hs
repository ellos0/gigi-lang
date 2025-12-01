module Structures (Expr(Atom, List), Statement(Assignment, Defun, Application, Add, Subtract, Multiply, Divide, Power), CompileError(SyntaxError)) where

data Expr
  = Atom String
  | List [Expr]
  deriving (Eq,Show)

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
  deriving (Show)

data CompileError
  = SyntaxError String
  deriving (Show)
