module MenuIO
    ( menu,
      splitTag,
      readTag
    ) where

import           Data.List.Split
import           Regex

type Tag = (String, Regex)

--Menu de Interacao com o usuario
menu :: [Tag] -> IO ()
menu tags = do
  str <- getLine
  let op    = take 2 str
      file  = drop 3 str
  case op of
    ":l" -> putStrLn ":l Falta Implementar" >> menu tags
    ":f" -> putStrLn ":f Falta Implementar" >> menu tags
    ":o" -> putStrLn ":o Falta Implementar" >> menu tags
    ":p" -> putStrLn ":p Falta Implementar" >> menu tags
    ":q" -> putStrLn "[INFO] Fim do programa"
    ":s" -> saveTag file tags               >> menu tags
    _    -> do
      let tag = readTag str
      case tag of
        Right s -> do
          putStrLn s
          menu tags
        Left t -> do
          putStrLn "[INFO] Nova definicao de tag carregada"
          menu (tags ++ [t])

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

-- Separa uma tag em nome e conteudo de acordo com a posicao de ": "
splitTag :: String -> String -> (String, String)
splitTag s str = (tag,expr)
  where
    spl  = splitOn s str
    tag  = head spl
    len  = length $ s ++ tag
    expr = drop len str

tag2str :: [Tag] -> String
tag2str []         = []
tag2str ((s,t):ts) = s ++ ": " ++ printRePol t ++ "\n" ++ tag2str ts

saveTag :: String -> [Tag] -> IO ()
saveTag file []   = putStrLn "[WARNING] Não existem tags"
saveTag file tags = writeFile file $ tag2str tags
