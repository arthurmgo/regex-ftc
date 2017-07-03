module Token where

data Token =  TChar Char
            | TPlus
            | TMinus
            | TTimes
            | TDot
            | TLambda
            | TEnter
            | TBar
            | TError  -- Tratar caracteres invalidos
              deriving (Show, Eq)


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


printToken :: Token -> String
printToken (TChar c) =  [c]
printToken TPlus     =  "+"
printToken TMinus    =  "-"
printToken TTimes    =  "*"
printToken TDot      =  "."
printToken TLambda   =  "\\l"
printToken TEnter    =  "\\n"
printToken TBar      =  "\\\\"


token2str :: [Token] -> String
token2str = concatMap printToken
