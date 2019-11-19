module NfaMatch (nfaMatch) where

import Nfa
import Set

nfaMatch :: Nfa Int -> String -> Bool
nfaMatch (Nfa qs moves q0 fin) str = not (empty == (trans (Nfa qs moves q0 fin) str `intersection` fin))


foldNFA :: (Set a -> Char -> Set a) -> Set a -> String -> Set a
foldNFA f r [] = r
foldNFA f r (c:cs) = foldNFA f (f r c) cs

trans :: Ord a => Nfa a -> String -> Set a

trans (Nfa qs moves q0 fin) str = foldNFA step startset str
		 where
		 step set ch = onetrans (Nfa qs moves q0 fin) set ch
		 startset = epsilonClosure (Nfa qs moves q0 fin) (singleton q0)

--This problem took me 1h30m to complete
