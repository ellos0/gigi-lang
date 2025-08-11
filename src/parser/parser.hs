import Text.Regex

data Token = Word String
           | Number Integer
           | TokenOperator Char
           | WhitespaceType Int
           deriving (Show, Eq)

type Tokens = [Token]

keywords :: [String]
keywords = ["new", "push", "remove", "slice"]

operators :: String
operators = "+-*/%:"

tokenize :: String -> Tokens
tokenize xs = [] 

main :: IO ()
main = do
  print "Hello, world!"
