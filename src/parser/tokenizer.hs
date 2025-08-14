{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Tokenizer (Token,Tokens,tokenize,getTokenType) where

import Constants (operators)
import Data.Char (isAlpha,isDigit)

data Token = Word String
           | Number Int
           | Operator Char
           | Whitespace Int
           deriving (Show, Eq)

type Tokens = [Token]

getTokenType :: Token -> String
getTokenType (Word _) = "Word"
getTokenType (Number _) = "Number"
getTokenType (Operator _) = "Operator"
getTokenType (Whitespace _) = "Whitespace"

wordToToken :: String -> Token
wordToToken x 
  | (head x) `elem` operators =  (Operator (head x))
  | isDigit (head x) =  ( Number (read x :: Int)) 
  | otherwise = Word (x)

wordsToTokens :: [String] -> Tokens
wordsToTokens x = (map wordToToken x) 

tokenize :: String -> Tokens
tokenize = wordsToTokens . words

main :: IO ()
main = do
  print "The tokenizer says hi"
 
