module Token where

data Token =  TChar Char
            | Plus
            | Lambda
            | Enter
            | Bar
              deriving (Show, Eq)

str2token :: String -> [Token]
str2token [] = []
str2token (x:y:xs) = case x of
  '\\' -> case y of
      '+' -> Plus   : str2token xs
      'n' -> Enter  : str2token xs
      'l' -> Lambda : str2token xs
      _   -> Bar    : str2token (y:xs)
  b -> TChar b : str2token (y:xs)
str2token [x] = case x of
  '\\' -> [Bar]
  a   -> [TChar a]



main :: IO ()
main = do
  a <- readFile "teste.txt"
  let b = str2token a
  mapM_ print b
