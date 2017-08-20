module NfaTypes
    (
    ) where

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
