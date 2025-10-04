module Compiler.Lexer (Token,Lex) where

data Token
  = Eof
  | Period
  | Comma
  | String
  | Plus
  | Minus
  | Asterisk
  | Slash
  | LParen
  | RParen
  | NumberInt
  | Word

data TokenValue
  = TokenInt Int
  | TokenChar Char
  | TokenString String

data TokenData  TokenData { token :: Token,
                            value :: TokenValue
                          }

getTokenDataValue :: TokenData -> TokenValue
getTokenDataValue x = value x

getTokenDataType :: TokenData -> Token
getTokenDataType x = token x

lexOne :: String -> Token