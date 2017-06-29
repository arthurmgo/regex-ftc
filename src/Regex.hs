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

printRe :: Regex -> String
printRe Lambda         = "@"
printRe (Lit a)        = [a]
printRe (Union r1 r2)  = "(" ++ printRe r1 ++ "+" ++ printRe r2 ++ ")"
printRe (Concat r1 r2) = "(" ++ printRe r1 ++ printRe r2 ++ ")"
printRe (Kleene r)     = "(" ++ printRe r ++ ")*"

charToString :: Char -> String
charToString c = [c]

chars :: String -> [String]
chars = map charToString

str2regex :: String -> Regex
str2regex = head . foldl foldFunction [] . chars
    where
        foldFunction (x:ys) "*"   = Kleene x : ys
        foldFunction (x:y:ys) "+" = Union y x : ys
        foldFunction (x:y:ys) "." = Concat y x : ys
        foldFunction xs [a]       = Lit a : xs


instance Show Regex where
  show = printRe
