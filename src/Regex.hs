module Regex
    (Regex,
     printRePol,
     printRe,
     str2regex
    ) where

import           Token

data Regex =  Lambda
            | Lit Char
            | Union Regex Regex
            | Concat Regex Regex
            | Kleene Regex
             deriving (Eq)


printRePol :: Regex -> String
printRePol Lambda         = "\\l"
printRePol (Lit '\n')     = "\\n"
printRePol (Lit '+')      = "\\+"
printRePol (Lit '-')      = "\\-"
printRePol (Lit '.')      = "\\."
printRePol (Lit '*')      = "\\*"
printRePol (Lit a)        = [a]
printRePol (Union r1 r2)  = printRePol r1 ++ printRePol r2 ++ "+"
printRePol (Concat r1 r2) = printRePol r1 ++ printRePol r2 ++ "."
printRePol (Kleene r)     = printRePol r ++ "*"


printRe :: Regex -> String
printRe Lambda         = "\\l"
printRe (Lit '\n')     = "\\n"
printRe (Lit '+')      = "\\+"
printRe (Lit '-')      = "\\-"
printRe (Lit '.')      = "\\."
printRe (Lit '*')      = "\\*"
printRe (Lit a)        = [a]
printRe (Union r1 r2)  = "(" ++ printRe r1 ++ "+" ++ printRe r2 ++ ")"
printRe (Concat r1 r2) = "(" ++ printRe r1 ++ printRe r2 ++ ")"
printRe (Kleene r)     = "(" ++ printRe r ++ ")*"


str2regex :: String -> Either Regex String
str2regex str =
  if null $ tail $ resp str then
     Left $ head $ resp str else
     Right "[ERROR] Nao constitui uma expressão regular valida"
     where
        resp = foldl foldFunction [] . str2token
        foldFunction (x:xs)   (TChar '*') = Kleene x   : xs
        foldFunction (x:y:xs) (TChar '+') = Union y x  : xs
        foldFunction (x:y:xs) (TChar '.') = Concat y x : xs
        foldFunction    xs    TPlus       = Lit '+'    : xs
        foldFunction    xs    TMinus      = Lit '-'    : xs
        foldFunction    xs    TTimes      = Lit '*'    : xs
        foldFunction    xs    TBar        = Lit '\\'   : xs
        foldFunction    xs    TEnter      = Lit '\n'   : xs
        foldFunction    xs    TLambda     = Lambda     : xs
        foldFunction    xs    TDot        = Lit '.'    : xs
        foldFunction    xs    (TChar a)   = Lit a      : xs


instance Show Regex where
  show = printRe
