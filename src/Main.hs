module Main where

import Lexer
import Codegen

emitC :: String -> [String]
emitC xs = map codegenStatement (parseStringClean xs) 

main :: IO ()
main = putStrLn "Hello, Haskell!"
