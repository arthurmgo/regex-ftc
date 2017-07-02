module MenuIO
    ( menu,
      splitTag,
      readTag
    ) where

import           Data.List.Split
import           Regex

type Tag = (String, Regex)

menu :: [Tag] -> IO ()
menu tag = do
  str <- getLine
  let op    = take 2 str
      file  = drop 3 str
  case op of
    ":l" -> putStrLn "FIM DO PROGRAMA"
    ":f" -> putStrLn "FIM DO PROGRAMA"
    ":o" -> putStrLn "FIM DO PROGRAMA"
    ":p" -> putStrLn "FIM DO PROGRAMA"
    ":q" -> putStrLn "FIM DO PROGRAMA"
    ":s" -> putStrLn "FIM"
    _    -> return ()

-- Tratamento de erro de string sem nome, e tag sem conteudo
readTag :: String -> Either Tag String
readTag str = case tag of
  ([],_) -> Right "[ERROR] Tag informada não possui nome"
  (_,[]) -> Right "[ERROR] Tag informada não possui conteudo"
  (a,b)  -> case reg of
    Right s -> Right s
    Left ex -> Left (a, ex)
  where
    tag = splitTag ": " str
    reg = str2regex $ snd tag



splitTag :: String -> String -> (String, String)
splitTag s str = (tag,expr)
  where
    spl  = splitOn s str
    tag  = head spl
    len  = length $ s ++ tag
    expr = drop len str
