module MenuIO
    ( menu
    ) where

import           Regex

type Tag = (String, String)

split :: String -> (String, String)
split = span (== ' ')

menu :: [Tag] -> IO ()
menu tag = do
  str <- getLine
  let op    = take 2 str
      file  = drop 3 str
  case op of
    ":l"      -> putStrLn "FIM DO PROGRAMA"
    ":f"      -> putStrLn "FIM DO PROGRAMA"
    ":o"      -> putStrLn "FIM DO PROGRAMA"
    ":p"      -> putStrLn "FIM DO PROGRAMA"
    ":q"      -> putStrLn "FIM DO PROGRAMA"
    ":s"      -> putStrLn "FIM DO PROGRAMA"
    _         -> return ()
