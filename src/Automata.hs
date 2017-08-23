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

states :: Nfa a -> Set a
states (NFA s mov start final) = s

moves :: Nfa a -> Set (Move a)
moves (NFA s mov start final) = mov

finishstates :: Nfa a -> Set a
finishstates (NFA s mov start final) = final

trans :: Ord a => Nfa a -> String -> Set a
trans mach = foldl step startset
      where
        step set ch = onetrans mach ch set
        startset = closure mach (sing (startstate mach))

trans2 :: Ord a => Nfa a -> String -> [(String,Set a)]
trans2 mach = transAux mach ""
  where
    transAux mach str []     = [(str,trans mach str)]
    transAux mach str (y:ys) = (str,trans mach str) : transAux mach (str ++ [y]) ys


{-lastMatch :: Ord a =>  Nfa a -> String -> Maybe (String,Set a)
lastMatch mach str  = lastM
    where
      fim      = finishstates mach
      matches  = trans2 mach str
      notEmpty = filter (not . isEmpty . inter fim . snd) matches
      lastM    = if null notEmpty
        then Nothing
        else Just (last notEmpty)-}

lastMatch :: Ord a =>  Nfa a -> String -> (String,Set a)
lastMatch mach str  = lastM
    where
      fim      = finishstates mach
      matches  = trans2 mach str
      notEmpty = filter (not . isEmpty . inter fim . snd) matches
      lastM    = if null notEmpty
        then ("", empty)
        else last notEmpty

onetrans :: Ord a => Nfa a -> Char -> Set a -> Set a
onetrans mach c x = closure mach (onemove mach c x)

onetrans2 :: Ord a => Nfa a -> Char -> Set a -> (Set a,Set a)
onetrans2 mach@(NFA states moves start term) c x
    = (r,f)
      where
        r = closure mach (onemove mach c x)
        f = inter r term


onemove :: Ord a => Nfa a -> Char -> Set a -> Set a
onemove (NFA states moves start term) c x
    = makeSet [s | t <- flatten x, Move z d s <- flatten moves, z == t, c == d]

closure :: Ord a => Nfa a -> Set a -> Set a
closure (NFA states moves start term) = setlimit add
    where
      add stateset = stateset `union` makeSet accessible
        where
          accessible
            = [s | x <- flatten stateset, Emove y s <- flatten moves, y == x]

printNfa :: (Show a) => Nfa a -> String
printNfa (NFA states moves start finish)
      = "States:\t" ++ showStates (flatten states)         ++ "\n" ++
        "Moves:\n"  ++ concatMap printMove (flatten moves) ++ "\n" ++
        "Start:\t"  ++ show start                          ++ "\n" ++
        "Finish:\t" ++ showStates (flatten finish)         ++ "\n"

showStates :: (Show a) => [a] -> String
showStates = concatMap ((++ " ") . show)

printMove :: (Show a) => Move a -> String
printMove (Move s1 c s2) = "\t" ++ show s1 ++ "----(" ++ [c] ++ ")---->" ++ show s2 ++ "\n"
printMove (Emove s1 s2)  = "\t" ++ show s1 ++ "----(@)---->" ++ show s2 ++ "\n"
