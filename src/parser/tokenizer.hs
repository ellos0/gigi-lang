{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Tokenizer (Token,Tokens,tokenize,getTokenType,LiteralValue) where

import Constants (operators, keywords)
import Data.Char (isAlpha,isDigit)

data LiteralValue = LiteralChar Char
                  | LiteralInt Int
                  | LiteralString [Char]
                  deriving (Show, Eq)

data Token = Word String
           | KeyWord String
           | Literal LiteralValue
           | Operator Char
           | Whitespace Int
           deriving (Show, Eq)

type Tokens = [Token]

getTokenType :: Token -> String
getTokenType (KeyWord _) = "KeyWord"
getTokenType (Word _) = "Word"
getTokenType (Literal _) = "Literal"
getTokenType (Operator _) = "Operator"
getTokenType (Whitespace _) = "Whitespace"

wordToToken :: String -> Token
wordToToken x 
  | x `elem` keywords = (KeyWord (x))
  | (head x) `elem` operators =  (Operator (head x))
  | isDigit (head x) =  ( Literal (LiteralInt (read x :: Int))) 
  | otherwise = Word (x)

wordsToTokens :: [String] -> Tokens
wordsToTokens x = (map wordToToken x) 

tokenize :: String -> Tokens
tokenize = wordsToTokens . words

main :: IO ()
main = do
  print ("hi")
 
