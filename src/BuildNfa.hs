module BuildNfa where

import           Automata
import           Regex
import           Sets

build :: Regex -> Nfa Int
build Lambda = NFA
          (makeSet [0 .. 1])
          (sing (Emove 0 1))
           0
          (sing 1)
build (Lit c) = NFA
          (makeSet [0 .. 1])
          (sing (Move 0 c 1))
           0
          (sing 1)
build (Union  r1 r2) = mOr   (build r1) (build r2)
build (Concat r1 r2) = mThen (build r1) (build r2)
build (Kleene r)     = mStar (build r)


mOr :: Nfa Int -> Nfa Int -> Nfa Int
mOr (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
  = NFA (states1' `union` states2' `union` newstates)
        (moves1' `union` moves2' `union` newmoves)
         0
        (sing (m1+m2+1))
   where
     m1        = card states1
     m2        = card states2
     states1'  = mapSet (renumber 1) states1
     states2'  = mapSet (renumber (m1 + 1)) states2
     newstates = makeSet [0, m1 + m2 + 1]
     moves1'   = mapSet (renumberMove 1) moves1
     moves2'   = mapSet (renumberMove (m1 + 1)) moves2
     newmoves  = makeSet [Emove 0 1, Emove 0 (m1+1), Emove m1 (m1 + m2+1), Emove (m1+m2) (m1+m2+1)]

-- Até aqui OK!

mThen :: Nfa Int -> Nfa Int -> Nfa Int
mThen (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
  = NFA (states1 `union` states2') (moves1 `union` moves2')
      start1
      finish2'
        where
          states2' = mapSet (renumber k) states2
          moves2'  = mapSet (renumberMove k) moves2
          finish2' = mapSet (renumber k) finish2
          k        = card states1 - 1

mStar :: Nfa Int -> Nfa Int
mStar (NFA states moves start finish)
    = NFA
      (states' `union` newstates)
      (moves' `union` newmoves)
       0
      (sing (m+1))
        where
          m         = card states
          states'   = mapSet  (renumber 1) states
          moves'    = mapSet  (renumberMove 1) moves
          newstates = makeSet [ 0 , m+1 ]
          newmoves  = makeSet [ Emove 0 1 , Emove m 1 , Emove 0 (m+1) , Emove m (m+1) ]


{-mOrFinish :: Nfa Int -> Nfa Int -> (Nfa Int, (Set Int,Set Int))
mOrFinish (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
  = (NFA (states1' `union` states2' `union` newstates)
         (moves1' `union` moves2' `union` newmoves)
          0
         (sing (m1+m2+1)),
         (finish1',finish2'))
   where
     m1        = card states1
     m2        = card states2
     finish1'  = mapSet (renumber (m1 + 1)) finish1
     finish2'  = mapSet (renumber (m2 + 1)) finish2
     states1'  = mapSet (renumber 1) states1
     states2'  = mapSet (renumber (m1 + 1)) states2
     moves1'   = mapSet (renumberMove 1) moves1
     moves2'   = mapSet (renumberMove (m1 + 1)) moves2
     newstates = makeSet [0, m1 + m2 + 1]
     newmoves  = makeSet [Emove 0 1, Emove 0 (m1+1), Emove m1 (m1 + m2+1), Emove (m1+m2) (m1+m2+1)]-}




renumber :: Int -> Int -> Int
renumber n st = n + st

renumberMove :: Int -> Move Int -> Move Int
renumberMove k (Move s1 c s2) = Move  (renumber k s1) c (renumber k s2)
renumberMove k (Emove s1 s2)  = Emove (renumber k s1)   (renumber k s2)
