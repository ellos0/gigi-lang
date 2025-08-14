module Constants (keywords,operators) where

operators :: String
operators = "+-*/%:"

keywords :: [String]
keywords = ["push","pull","get","set","length"]

lookupTable :: [(Either Char String,Int)]
lookupTable = []
