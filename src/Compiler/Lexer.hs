module Compiler.Lexer where

--splitByNewlines :: String -> [String]
--splitByNewLines = lines

--splitBySpaces :: String -> [String]
--splitBySpaces = words

data Statement
  = Assignment String String
  | Add String String
  | Subtract String String
  | Multiply String String
  | Divide String String
  | Function String [Statement]

processSingleLine :: [String] -> String
processSingleLine (x:xs)
    | x == "=" = "Assignment"
    | x == "+" = "Add"
    | x == "-" = "Minus"
    | x == "*" = "Multiply"
    | x == "/" = "Divide"
    | x == "defun" = "Function"
    | otherwise = "Unknown"