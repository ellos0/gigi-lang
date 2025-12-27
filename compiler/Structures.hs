module Structures where

import Data.Char (isNumber) 

stringIsInteger :: String -> Bool
stringIsInteger [] = False
stringIsInteger (x:xs) = case x of
  '-' -> stringIsPositiveInteger xs
  _ -> stringIsPositiveInteger (x:xs)

stringIsPositiveInteger :: String -> Bool
stringIsPositiveInteger x = foldr (&&) True (map isNumber x)

doubleQuote :: Char
doubleQuote = '"'

singleQuote :: Char
singleQuote = ("'") !! 0

data GigiVal
  = Atom String
  | StringVal String
  | List [GigiVal]
  | Command String GigiVal
  deriving (Eq,Show)

getAtomValue :: GigiVal -> Maybe String
getAtomValue (Atom s) = Just s
getAtomValue _ = Nothing

getAtomValuePartial :: GigiVal -> String
getAtomValuePartial (Atom s) = s
getAtomValuePartial _ = ""

firstOfExpr :: GigiVal -> Maybe String
firstOfExpr (List x) = getAtomValue (head x)
firstOfExpr _ = Nothing

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
  | StringLiteral String
  | IntLiteral String
  | Literal String
  | Multi [Statement]
  | CommandStatement String Statement
  | TypeDeclaration [Statement]
  | NullStatement
  | Error CompileError
  deriving (Show)

getLiteralValue :: Statement -> String
getLiteralValue (StringLiteral x) = x
getLiteralValue (IntLiteral x) = x
getLiteralValue (Literal x) = x
getLiteralValue _ = ""

extractType :: Statement -> [String]
extractType x = case x of
                  (TypeDeclaration xs) -> map (getLiteralValue) xs
                  _ -> ["auto"]

newline :: Char
newline = '\n'

newlineArray :: String
newlineArray = [newline]

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
