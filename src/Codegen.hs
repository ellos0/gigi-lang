module Codegen where

import Lexer

addSemicolon :: String -> String
addSemicolon x = x ++ ";"

convertErrorStatement :: Either CompileError Statement -> Maybe Statement
convertErrorStatement Right x = Just x
convertErrorStatement _ = Nothing

codegenFunction :: Expr -> String
codegenFunction _ = "" --temporary! remove soon!

codegenAtomOrStatement :: Expr -> String
codegenAtomOrStatement (Atom x) = x
codegenAtomOrStatement x = codegenStatement x

codegenStatement :: Statement -> String
codegenStatement (Add x1 x2) = (codegenAtomOrStatement x1) ++ " + " ++ (codegenAtomOrStatement x2)
codegenStatement (Subtract x1 x2) = (codegenAtomOrStatement x1) ++ " - " ++ (codegenAtomOrStatement x2)
codegenStatement (Multiply x1 x2) = (codegenAtomOrStatement x1) ++ " * " ++ (codegenAtomOrStatement x2)
codegenStatement (Divide x1 x2) = (codegenAtomOrStatement x1) ++ " / " ++ (codegenAtomOrStatement x2)
codegenStatement (Power x1 x2) = (codegenAtomOrStatement x1) ++ " ^ " ++ (codegenAtomOrStatement x2)
codegenStatement (Assignment x1 x2) = "auto " ++ x1 ++ " = " ++ (codegenAtomOrStatement x2)
--codegenStatement (Defun x1 x2) = "auto " ++ x1 ++ " = " ++ (codegenAtomOrStatement x2)
codegenStatement (Application x1) = codegenAtomOrStatement x1 ++ "()"
