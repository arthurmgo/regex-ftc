-- Arthur Miranda Gomes - 14.1.8338
-- Pedro Henrique Mendes Batista  - 14.1.8403
module Token where

-- Tipo de dado que define uma representação simbolica para caracteres
data Token =  TChar Char
            | TPlus
            | TMinus
            | TTimes
            | TDot
            | TLambda
            | TEnter
            | TBar
            | TError  -- Tratar caracteres invalidos - exemplo: \a
              deriving (Show, Eq)

-- Mapeia uma string em uma lista de Simbolos equivalentes
str2token :: String -> [Token]
str2token [] = []
str2token (x:y:xs) = case x of
  '\\' -> case y of
      '+' -> TPlus   : str2token xs
      '-' -> TMinus  : str2token xs
      '*' -> TTimes  : str2token xs
      '.' -> TDot    : str2token xs
      'n' -> TEnter  : str2token xs
      'l' -> TLambda : str2token xs
      '\\'-> TBar    : str2token xs
      _   -> TError  : str2token xs
  b -> TChar b : str2token (y:xs)
str2token [x] = case x of
  '\\' -> [TError]
  a    -> [TChar a]

-- Representação de um Simbolo em uma String
printToken :: Token -> String
printToken (TChar c) =  [c]
printToken TPlus     =  "+"
printToken TMinus    =  "-"
printToken TTimes    =  "*"
printToken TDot      =  "."
printToken TLambda   =  "\\l"
printToken TEnter    =  "\\n"
printToken TBar      =  "\\\\"

-- Converte uma lista de Simbolos em uma String
token2str :: [Token] -> String
token2str = concatMap printToken
