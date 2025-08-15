module Constants (keywords,operators,keywordToOperation,lookupTable) where

import Data.Maybe (fromJust)

operators :: String
operators = "+-*/%:"

keywords :: [String]
keywords = ["push","pull","get","set","length"]

lookupTable :: [(String,Int)]
lookupTable = [("push", 0)]

keywordToOperation :: String -> Int
keywordToOperation x = fromJust (lookup x lookupTable)

main :: IO ()
main = do
  print "greetings from the constants"
