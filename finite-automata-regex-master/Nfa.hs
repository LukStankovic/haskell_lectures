module Nfa (
    Nfa(..),
    Dfa,
    Move(..),

    alphabet,
    nfaSize,

    epsilonAccessible,
    epsilonClosure,

    onemove,
    onetrans,

    mapNfa,
    filterNfa,
    numberNfaFrom
  ) where

import Data.List (nub, sort)

import Fixpoint
import Set

data Nfa q = Nfa (Set q) (Set (Move q)) q (Set q)
  deriving (Eq, Show, Read)

data Move q = Emove q q
            | Move q Char q
  deriving (Eq, Ord, Show, Read)

type Dfa q = Nfa q

-- return the number of states in the NFA
nfaSize :: Nfa q -> Int
nfaSize (Nfa qs _ _ _) = size qs

-- Return the alphabet of the machine by finding a list of the symbols mentioned
-- in the machine's moves.
alphabet :: Nfa q -> [Char]
alphabet (Nfa _ moves _ _) =
    nub $ sort [c | Move _ c _ <- toList moves]

-- Given an Nfa and a set of states Q, return the set of states that are
-- accessible from Q via epsilon transitions.
epsilonAccessible :: Ord q => Nfa q -> Set q -> Set q
epsilonAccessible (Nfa _ moves _ _) qs =
    fromList accessible
  where
    accessible = [r | q <- toList qs,
                      Emove q' r <- toList moves,
                      q == q']

-- Given an Nfa and a set of states Q, return the set of states that are
-- accessible from Q via zero or more epsilon transitions.
epsilonClosure :: Ord q => Nfa q -> Set q -> Set q
epsilonClosure nfa qs0 =
    fixpoint addEpsilonAccessible qs0
  where
    addEpsilonAccessible qs = qs `union` epsilonAccessible nfa qs

-- Return the set of states accessible from a given set by a single move on the
-- given symbol
onemove :: Ord q => Nfa q -> Set q -> Char -> Set q
onemove (Nfa _ moves _ _) qs c =
    fromList [s | q <- toList qs,
                  Move r c' s <- toList moves,
                  r == q,
                  c == c']

-- onetrans performs one move and then takes the epsilon closure of the result.
onetrans :: Ord q => Nfa q -> Set q -> Char -> Set q
onetrans nfa q c = epsilonClosure nfa (onemove nfa q c)

-- Map a function across the states of an Nfa
mapNfa :: (Ord q, Ord q') => (q -> q') -> Nfa q -> Nfa q'
mapNfa f (Nfa qs moves q0 fin) =
    Nfa (mapSet f qs) (mapSet (mapMove f) moves) (f q0) (mapSet f fin)

mapMove :: (q -> q') -> Move q -> Move q'
mapMove f (Emove q r)  = Emove (f q) (f r)
mapMove f (Move q c r) = Move (f q) c (f r)

filterNfa :: Ord q => (q -> Bool) -> Nfa q -> Nfa q
filterNfa f (Nfa qs moves q0 fin) =
    Nfa (filterSet f qs) (filterSet (filterMove f) moves) q0 (filterSet f fin)

filterMove :: (q -> Bool) -> Move q -> Bool
filterMove f (Emove q r)  = f q && f r
filterMove f (Move q _ r) = f q && f r

-- Number the states in an Nfa starting from i
numberNfaFrom :: Ord q => Int -> Dfa q -> Dfa Int
numberNfaFrom i nfa@(Nfa qs _ _ _) = mapNfa number nfa
  where
    stateMap = toList qs `zip` [i..]
    number q = look q stateMap

look :: Eq a => a -> [(a,b)] -> b
look _ []                       = error "look"
look k ((k',v):kvs) | k == k'   = v
                    | otherwise = look k kvs
