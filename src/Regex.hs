module Regex
    (
    ) where

data Regex = Lambda |
             Lit Char |
             Union Regex Regex |
             Concat Regex Regex |
             Kleene Regex
             deriving (Eq)


literals :: Regex -> String
literals Lambda         = []
literals (Lit a)        = [a]
literals (Union r1 r2)  = literals r1 ++ literals r2
literals (Concat r1 r2) = literals r1 ++ literals r2
literals (Kleene r)     = literals r

printRePol :: Regex -> String
printRePol (Lit a)       = [a]
printRePol (Union r1 r2) = printRePol r1 ++ printRePol r2 ++ "+"
