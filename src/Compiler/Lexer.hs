module Compiler.Lexer where

import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

type Var = String
data Statement = Assign Var Int | Function String [Int] deriving (Eq,Show)
type Code = [Statement]