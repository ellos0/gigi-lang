module Parser (parse, IR) where

import Constants (keywords, keywordToOperation)
import Tokenizer (Token,Tokens,tokenize,getTokenType,LiteralValue)


data IR = IR {op :: Int,  args :: Maybe [LiteralValue] } deriving (Show)

parseWord :: Token -> IR
parseWord x = IR (keywordToOperation "jmp") (Just (x))

parseOperator :: Token -> IR
parseOperator x = IR (keywordToOperation (charToString x))  (Nothing)
  where 
    charToString c = [c]

parseKeyWord :: [Token] -> IR
parseKeyWord 

parseTokenSingle :: Token -> IR
parseTokenSingle x = case getTokenType x of
  "Word" -> parseWord x
  "Operator" -> parseOperator x
  _ -> x

parseTokens :: Tokens -> [IR]
parseTokens xs = map parseTokenSingle xs

main :: IO ()
main = do
  print "hi from the parser"
