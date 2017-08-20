module BuildNfa
    (
    ) where

import Regex
import Automata

build :: Reg -> Nfa Int
build (Lit c)
  = NFA (makeSet [0 .. 1])
        (sing (Move 0 c 1))
        0
        (sing 1)
build (Union r1 r2) = mOr (build r1) (build r2)
build (Concat r1 r2) = mThen (build r1) (build r2)
build (Kleene r) = mStar (build r)


mOr :: Nfa Int -> Nfa Int -> Nfa Int
mOr (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
  = NFA (states1' 'union' states2' 'union' newstates)
        (moves1' 'union' moves2' 'union' newmoves)
         0
        (sing (m1+m2+1))
   where
     m1 = card states1
     m2 = card states2
     states1' = mapSet (renumber 1) states1
     states2' = mapSet (renumber (m1 + 1)) states2
     newstates = makeSet [0, m1 + m2 + 1]
     moves1' = mapSet (renumber_move 1) moves1
     moves2' = mapSet (renumber_move (m1 + 1)) moves2
     newmoves = makeSet [Emove 0 1, Emove 0 (m1+1), Emove m1 (m1 + m2+1), Emove (m1+m2) (m1+m2+1)]

mThen :: Nfa Int -> Nfa Int -> Nfa Int
mThen = undefined

mStar :: Nfa Int -> Nfa Int
mStar = undefined
