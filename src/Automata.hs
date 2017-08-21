module Automata where

import           Sets

data Nfa a = NFA (Set a)
                 (Set (Move a))
                  a
                 (Set a)
            deriving (Eq, Show)

data Move a = Move a Char a
             |Emove a a
             deriving (Eq, Ord, Show)

startstate :: Nfa a -> a
startstate (NFA s mov start final) = start

trans :: Ord a => Nfa a -> String -> Set a
trans mach
    = foldl step startset
        where
          step set ch = onetrans mach ch set
          startset = closure mach (sing (startstate mach))


onetrans :: Ord a => Nfa a -> Char -> Set a -> Set a
onetrans mach c x = closure mach (onemove mach c x)

onemove :: Ord a => Nfa a -> Char -> Set a -> Set a
onemove (NFA states moves start term) c x
    = makeSet [s | t <- flatten x,
                   Move z d s <- flatten moves,
                   z == t, c == d]

closure :: Ord a => Nfa a -> Set a -> Set a
closure (NFA states moves start term)
    = setlimit add
      where
        add stateset = stateset `union` makeSet accessible
          where
            accessible
              = [s | x <- flatten stateset,
                     Emove y s <- flatten moves,
                     y == x]


printNfa :: (Show a) => Nfa a -> String
printNfa (NFA states moves start finish)
      = "States:\t" ++ showStates (flatten states) ++ "\n" ++
        "Moves:\n" ++ concatMap printMove (flatten moves) ++ "\n" ++
        "Start:\t" ++ show start ++ "\n" ++
        "Finish:\t" ++ showStates (flatten finish) ++ "\n"

showStates :: (Show a) => [a] -> String
showStates = concatMap ((++ " ") . show)

printMove :: (Show a) => Move a -> String
printMove (Move s1 c s2) = "\t" ++ show s1 ++ "----(" ++ [c] ++ ")---->" ++ show s2 ++ "\n"
printMove (Emove s1 s2)  = "\t" ++ show s1 ++ "----(@)---->" ++ show s2 ++ "\n"
