{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Parser where

import Data.Maybe (catMaybes)
import Constants (operators, keywords)
import Data.Char (isAlpha,isDigit)

data LiteralValue = LiteralChar Char
                  | LiteralInt Int
                  | LiteralString [Char]
                  deriving (Show, Eq)

data Token = Word String
           | Keyword String
           | Literal LiteralValue
           | Operator Char
           | Whitespace Int
           deriving (Show, Eq)

type Tokens = [Token]

getTokenType :: Token -> String
getTokenType (Keyword _) = "Keyword"
getTokenType (Word _) = "Word"
getTokenType (Literal _) = "Literal"
getTokenType (Operator _) = "Operator"
getTokenType (Whitespace _) = "Whitespace"

wordToToken :: String -> Token
wordToToken x 
  | x `elem` keywords = (Keyword (x))
  | (head x) `elem` operators =  (Operator (head x))
  | isDigit (head x) =  ( Literal (LiteralInt (read x :: Int))) 
  | otherwise = Word (x)

wordsToTokens :: [String] -> Tokens
wordsToTokens x = (map wordToToken x) 

tokenize :: String -> Tokens
tokenize = wordsToTokens . words

data IR = IR {op :: Int,  args :: Maybe [LiteralValue] } deriving (Show)

parseWord :: Token -> IR
parseWord x = IR (keywordToOperation "jmp") (Just ([x]))

parseOperator :: Token -> IR
parseOperator x = IR (keywordToOperation (charToString x))  (Nothing)
  where 
    charToString c = [c]

parseLiteral :: Token -> IR
parseLiteral x = IR (keywordToOperation "push") (Just ([x]))


parseKeyword :: [Token] -> IR
parseKeyword (xs:x) = IR (keywordToOperation x) (Just (xs))


parseTokenSingle :: Token -> Maybe IR
parseTokenSingle x = case getTokenType x of
  "Word" -> Just (parseWord x)
  "Operator" -> Just (parseOperator x)
  "Literal" -> Just (parseLiteral x)
  _ -> Nothing

parseTokens :: Tokens -> [IR]
parseTokens xs = catMaybes (map (parseTokenSingle) xs)

main :: IO ()
main = do
  print "hi from the parser"
