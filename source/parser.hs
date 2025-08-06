import Data.Char (isSpace, isAlpha, isDigit)
import Data.Maybe (catMaybes)

data SubToken = Alpha Char
              | Num Integer
              | Whitespace Char
              | Operator Char
              deriving (Show,Eq)

data Token = Keyword String
           | Number Integer
           | Operator Char
           | Whitespace String
           deriving (Show,Eq)

type SubTokens = [SubToken]

type Tokens = [Token]

keywords :: [String]
keywords = ["new", "push", "remove", "slice"]

operators :: String
operators = "+-*/%:"

classifyChar :: Char -> Maybe SubToken
classifyChar c
  | c `elem` operators = Just (Operator c)
  | isAlpha c = Just (Alpha c)
  | isDigit c = Just (Num (fromIntegral (ord c)))
  | isSpace c = Just (Whitespace c)
  | otherwise Nothing

classifyChars :: String -> [Maybe SubToken]
classifyChars s = map classifyChar s

main :: IO ()

main = do
  print "Hello, world!"
