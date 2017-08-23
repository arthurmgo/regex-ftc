-- Arthur Miranda Gomes - 14.1.8338
-- Pedro Henrique Mendes Batista  - 14.1.8403
module MenuIO
    ( menu,
      splitTag,
      readTag,
      readTagFile
    ) where

import           Automata
import           BuildNfa
import           Data.Either
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Regex
import           Sets
import           System.Directory

type TagName =  String
type Tag     = (TagName, Regex)
type TagNfa  = (TagName, Nfa Int)

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

-- Leitura de um arquivo que contem Tags
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
splitTag :: String -> String -> (TagName, String)
splitTag s str = (tag,expr)
  where
    spl  = splitOn s str
    tag  = head spl
    len  = length $ s ++ tag
    expr = drop len str

tag2str :: [Tag] -> String
tag2str []         = []
tag2str ((s,t):ts) = s ++ ": " ++ printRePol t ++ "\n" ++ tag2str ts


--Salva tag em um arquivo
saveTag :: FilePath -> [Tag] -> IO ()
saveTag file []   = putStrLn "[WARNING] Não existem tags"
saveTag [] _      = putStrLn "[ERROR] Nome de arquivo invalido"
saveTag file tags = do
  writeFile file $ tag2str tags
  putStrLn "[INFO] Arquivo salvo com sucesso"


nextString :: String -> String -> String
nextString str = snd . splitAt (length str)

convertTag :: Tag -> TagNfa
convertTag (s,r) = (s, build r)

convertTags :: [Tag] -> [TagNfa]
convertTags = map convertTag

makeNfa :: [TagNfa] ->  (Nfa Int, [(TagName, Set Int)])
makeNfa tags = (fst f,mapfin)
    where
      f = mJoin (map snd tags)
      mapfin = zip (map fst tags) (snd f)


{-
apply :: Nfa Int -> String -> Either [Set Int] String
apply x y = if foldl' (==) True (map isNothing resp)
    then Right "[ERROR] Nao foi possivel avaliar toda string"
    else Left (map fromJust resp)
      where
        resp = f x y
        f nfa []  =  []
        f nfa str = if padrao == ""
          then [Nothing]
          else Just estado : f nfa nextstr
          where
            (padrao, estado) = lastMatch nfa str
            nextstr = nextString padrao str -}

divideString :: [Tag] -> String -> IO()
divideString t s = do
  let tagnfa = convertTags t
      (automato, mapa) = makeNfa tagnfa
      




f :: Nfa Int -> String ->  [Maybe (Set Int)]
f nfa []  =  []
f nfa str = if padrao == ""
    then [Nothing]
    else Just (inter fim estado) : f nfa nextstr
    where
      (padrao, estado) = lastMatch nfa str
      nextstr = nextString padrao str
      fim = finishstates nfa



{-
func :: [Tag] -> String -> [TagName]
func tags str@(x:xs) = ["a"]
    where
          (nfa, fim)   = makeNfa $ convertTags tags
          finish  = finishstates nfa
          apply   = lastMatch nfa str
          list = if isNothing apply
            then Nothing
            else fromJust apply
          name    = fst $ fromJust list
          patt    = snd $ fromJust list
          nextstr = nextString patt str-}

{-
findMax :: [Maybe (TagName, String)] -> Maybe (TagName, String)
findMax tag = maxTag (head tag) (tail tag)
    where
      maxTag x [] = x
      maxTag x (y:ys) = if length x >= length y
        then maxTag x ys
        else maxTag y ys


apply ::  String -> TagNfa -> Maybe (TagName, String)
apply str (name,mach)  = if isNothing resp
  then Nothing
  else Just (name, fst $ fromJust resp)
    where
      resp = lastMatch str mach

applyList :: String -> [TagNfa] ->  Maybe (TagName, String)
applyList str  = findMax . map (apply str)


mOrTag :: TagNfa -> TagNfa -> (Nfa Int,[(TagName, Set Int)])
mOrTag t1 t2 = (machor, (name1, finishstates mach1):[(name2,finishstates mach2)])
    where
      name1  = fst t1
      name2  = fst t2
      mach1  = snd t1
      mach2  = snd t2
      machor = mOr mach1 mach2
-}
