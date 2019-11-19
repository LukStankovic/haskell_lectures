module RegExpToNfa (regExpToNfa) where

import Nfa
import RegExp
import Set

-- NFAs built by regExpToNfa obey a few invariants that make them easier to work
-- with:
--
-- 1) All states are consecutively numbered from 0.
-- 2) State 0 is the start state
-- 3) The returned NFA has a single accept state, and it is the
--    highest-numbered state

regExpToNfa :: RegExp -> Nfa Int
regExpToNfa Empty =
    Nfa (fromList [0..1])
        empty
        0
        (singleton 1)

regExpToNfa Epsilon =
    Nfa (fromList [0..1])
        (singleton (Emove 0 1))
        0
        (singleton 1)

regExpToNfa (Lit c) =
    Nfa (fromList [0..1])
		  (singleton (Move 0 c 1))
        0
        (singleton 1)

regExpToNfa (Cat r s) =
    Nfa (qs1 `union` qs2)
        (moves1 `union` moves2 `union` fromList catMoves)
        start1
        (singleton (acceptState s_nfa))
   where
    r_nfa@(Nfa qs1 moves1 start1 _) = 
        numberNfaFrom 0 $ regExpToNfa r

    s_nfa@(Nfa qs2 moves2 start2 _) =
        numberNfaFrom (nfaSize r_nfa) $ regExpToNfa s

    catMoves = [Emove (acceptState r_nfa) start2]

regExpToNfa (Alt r s) =
    Nfa (qs1    `union` qs2    `union` fromList [q0,qf])
        (moves1 `union` moves2 `union` fromList altMoves)
        0
        (singleton qf)
  where
    r_nfa@(Nfa qs1 moves1 start1 _) =
        numberNfaFrom 1 $ regExpToNfa r

    s_nfa@(Nfa qs2 moves2 start2 _) =
        numberNfaFrom (nfaSize r_nfa + 1) $ regExpToNfa s

    q0 = 0
    qf = nfaSize r_nfa + nfaSize s_nfa + 1

    altMoves = [ Emove q0 start1
               , Emove q0 start2
               , Emove (acceptState r_nfa) qf
               , Emove (acceptState s_nfa) qf]

regExpToNfa (Star r) =
    Nfa (qs `union` fromList [q0,qf])
        (moves `union` fromList starMoves)
        0
        (singleton qf)
  where
    r_nfa@(Nfa qs moves start _) =
        numberNfaFrom 1 $ regExpToNfa r

    q0 = 0
    qf = nfaSize r_nfa + 1

    starMoves = [ Emove q0 start
                , Emove q0 qf
                , Emove (acceptState r_nfa) qf
                , Emove (acceptState r_nfa) start]

acceptState :: Nfa q -> q
acceptState (Nfa _ _ _ f) =
    case toList f of
      [q] -> q
      _   -> error "acceptState: NFA does not have a single accept state"
