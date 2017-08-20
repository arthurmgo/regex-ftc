-- Arthur Miranda Gomes - 14.1.8338
-- Pedro Henrique Mendes Batista  - 14.1.8403
module Regex
    (Regex(..),
     printRePol,
     printRe,
     str2regex
    ) where

import           Token

-- Tipo de dado para representar uma expressão regular
data Regex =  Lambda
            | Lit Char
            | Union Regex Regex
            | Concat Regex Regex
            | Kleene Regex
             deriving (Eq)

-- Conversão de uma expressão regular para uma string equivalente em RPN
printRePol :: Regex -> String
printRePol Lambda         = "\\l"
printRePol (Lit '\n')     = "\\n"
printRePol (Lit '+')      = "\\+"
printRePol (Lit '-')      = "\\-"
printRePol (Lit '.')      = "\\."
printRePol (Lit '*')      = "\\*"
printRePol (Lit '\\')     = "\\\\"
printRePol (Lit a)        = [a]
printRePol (Union r1 r2)  = printRePol r1 ++ printRePol r2 ++ "+"
printRePol (Concat r1 r2) = printRePol r1 ++ printRePol r2 ++ "."
printRePol (Kleene r)     = printRePol r ++ "*"

-- Conversão de uma expressão regular para uma string equivalente
printRe :: Regex -> String
printRe Lambda         = "\\l"
printRe (Lit '\n')     = "\\n"
printRe (Lit '+')      = "\\+"
printRe (Lit '-')      = "\\-"
printRe (Lit '.')      = "\\."
printRe (Lit '*')      = "\\*"
printRe (Lit '\\')     = "\\\\"
printRe (Lit a)        = [a]
printRe (Union r1 r2)  = "(" ++ printRe r1 ++ "+" ++ printRe r2 ++ ")"
printRe (Concat r1 r2) = "(" ++ printRe r1 ++ printRe r2 ++ ")"
printRe (Kleene r)     = "(" ++ printRe r ++ ")*"

-- Conversão de uma strinf em RPN em uma Expressão regular, caso exista erros retorna mensagem
str2regex :: String -> Either Regex String
str2regex str
    | TError `elem` str2token str =  -- Se a string possui um erro sintatico
      Right "[ERROR] Formato irregular de String"
    | null (tail $ solveRPN str) = -- Se a string foi completamente avaliada
      Left $ head $ solveRPN str
    | otherwise =       -- Se não foi possivel avaliar a string por completo
      Right "[ERROR] Nao constitui uma expressao regular valida"
     where
        -- transforma uma string em RPN em uma Regex
        solveRPN = foldl foldFunction [] . str2token
        -- Operação realizada de acordo com caractere lido
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
