{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Tokenizer (Token,tokenize) where

import Data.Char (isAlpha,isDigit)

data Token = Word String
           | Number Integer
           | Operator Char
           | WhitespaceType Int
           deriving (Show, Eq)

type Tokens = [Token]

operators :: String
operators = "+-*/%:"

wordToToken :: String -> Token
wordToToken x 
  | (head x) `elem` operators =  (Operator (head x))
  | isDigit (head x) =  ( Number (read x :: Integer)) 
  | otherwise = Word (x)

wordsToTokens :: [String] -> Tokens
wordsToTokens x = (map wordToToken x) 

tokenize :: String -> Tokens
tokenize = wordsToTokens . words

main :: IO ()
main = do
  print "Hello, world!"
