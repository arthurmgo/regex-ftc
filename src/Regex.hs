module Regex
    (
    ) where

data Regex = Lambda |
             Lit Char |
             Union Regex Regex |
             Concat Regex Regex |
             Kleene Regex
             deriving (Eq)


printRePol :: Regex -> String
printRePol Lambda         = "@"
printRePol (Lit a)        = [a]
printRePol (Union r1 r2)  = printRePol r1 ++ printRePol r2 ++ "+"
printRePol (Concat r1 r2) = printRePol r1 ++ printRePol r2 ++ "."
printRePol (Kleene r)     = printRePol r ++ "*"

str2regex :: String -> Regex
str2regex = undefined
