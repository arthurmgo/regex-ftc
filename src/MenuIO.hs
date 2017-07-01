module MenuIO
    ( menu,
      splitTag
    ) where

import           Data.List.Split
import           Regex

type Tag = (String, String)

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


splitTag :: String -> String -> (String, String)
splitTag s str = (tag,expr)
  where
    spl  = splitOn s str
    tag  = head spl
    len  = length $ s ++ tag
    expr = drop len str
