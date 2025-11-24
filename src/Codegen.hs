module Gir where

addSemicolon :: String -> String
addSemicolon x = x ++ ";"

codegenFunction :: Statement -> Statement -> String
codegenFunction _ _ = "" --temporary! remove soon!

codegenStatement :: Statement -> String
codegenStatement (Add x1 x2) = (codegenStatement x1) ++ " + " ++ (codegenStatement x2)
codegenStatement (Subtract x1 x2) = (codegenStatement x1) ++ " - " ++ (codegenStatement x2)
codegenStatement (Multiply x1 x2) = (codegenStatement x1) ++ " * " ++ (codegenStatement x2)
codegenStatement (Divide x1 x2) = (codegenStatement x1) ++ " / " ++ (codegenStatement x2)
codegenStatement (Power x1 x2) = (codegenStatement x1) ++ " ^ " ++ (codegenStatement x2)
codegenStatement (Assignment x1 x2) = "auto " ++ x1 ++ " = " ++ (codegenStatement x2)
--codegenStatement (Defun x1 x2) = (codegenFunction x1 x2)
codegenStatement (Application x1) = codegenStatement x1 ++ "()"

data Statement
  = Assignment String Statement
  | Defun String Statement
  | Application Statement 
  | Add Statement Statement
  | Subtract Statement Statement
  | Multiply Statement Statement
  | Divide Statement Statement
  | Power Statement Statement
  deriving (Show)
