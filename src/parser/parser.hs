module Parser (parse, IR) where

import Constants (keywords)
import Tokenizer (Token,Tokens,tokenize,getTokenType)

data Arg = IntOutput Int
         | FloatOutput Float
         | CharOutput Char
         | StringOutput String
         deriving (Show)

data IR = IR {op :: Int, args :: [Arg] } deriving (Show)

parseWord x 
  | x `elem` 

parseToken :: Token -> IR
parseToken x = case getTokenType x of
  "Word" -> parseWord x
  "Number" -> parseNumber x
  "Operator" -> parseOperator x
  "Whitespace" -> parseWhitespace x

parseTokens :: Tokens -> [IR]
parseTokens xs = map parseToken xs
