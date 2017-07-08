-- Arthur Miranda Gomes - 14.1.8338
-- Pedro Henrique Mendes Batista  - 14.1.8403
module MenuIO
    ( menu,
      splitTag,
      readTag,
      readTagFile
    ) where

import           Data.List.Split
import           Regex
import           System.Directory
import           Data.Either

type Tag = (String, Regex)

--Menu de Interacao com o usuario
menu :: [Tag] -> IO ()
menu tags = do
  str <- getLine         -- Leitura de uma linha
  let op    = take 2 str -- Divisão da string lida (operaçao)
      file  = drop 3 str -- Divisão da string lida (arquivo/texto)
  case op of
    ":l" -> readTagFile file               >>= menu
    ":f" -> putStrLn ":f Falta Implementar" >> menu tags
    ":o" -> putStrLn ":o Falta Implementar" >> menu tags
    ":p" -> putStrLn ":p Falta Implementar" >> menu tags
    ":s" -> saveTag file tags               >> menu tags
    ":q" -> putStrLn "[INFO] Fim do programa"
    _    -> do           -- Caso nenhuma operação seja reconhecida, tratar como leitura de tag
      let tag = readTag str
      case tag of
        Right s -> do
          putStrLn s
          menu tags
        Left t -> do
          putStrLn "[INFO] Nova definicao de tag carregada"
          menu (tags ++ [t])

-- Conversão de uma string para uma tag: (A: ab+) -> ("A",Or (Lit 'a') (Lit 'b'))
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

-- Leitura de um arquivo que co
readTagFile :: FilePath -> IO [Tag]
readTagFile [] = putStrLn "[ERROR] Nome de Arquivo Invalido" >> return []
readTagFile file = do
  exist <- doesFileExist file
  if exist
    then do
      str <- readFile file
      let tags = lefts $ map readTag $ lines str
          len1 = length $ lines str
          len2 = length tags
      putStrLn ("[INFO] Leitura de arquivo concluida. " ++ show (len1 - len2) ++ " tags não reconhecidas")
      return tags
    else do
      putStrLn "[ERROR] Arquivo não encontrado"
      return []


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

saveTag :: FilePath -> [Tag] -> IO ()
saveTag file []   = putStrLn "[WARNING] Não existem tags"
saveTag [] _      = putStrLn "[ERROR] Nome de arquivo invalido"
saveTag file tags = do
  writeFile file $ tag2str tags
  putStrLn "[INFO] Arquivo salvo com sucesso"
