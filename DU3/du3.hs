-- iterace, zřetězení, ... já 1 - dva týdny
-- http://behalek.cs.vsb.cz/wiki/index.php/FP_Homework_3_extension

-- regulářní výraz - spojení s tečkou a nebo, iterace, znak
-- potom udělat regulární výraz na automat
    --  reprezentace automatu - 0 startovní - dvojice trojice počáteční a stav
-- R(A) | R(B) - nový stav
-- R(A) . R(B) - koncové na ten počáteční
-- iterace - opakování



-- netradiční jazyk - referát - příští týden se ozvat

-- import Control.Monad (join)

import Control.Monad

-- data RegExp = Empty
--             | Epsilon
--             | Lit Char
--             | Cat RegExp RegExp
--             | Alt RegExp RegExp
--             | Star RegExp
--     deriving (Eq, Ord, Read)

-- test :: RegExp
-- test = Cat (Star(Alt (Lit 'a') (Lit 'b'))) (Lit 'c')

    
-- showExpr :: RegExp -> String
-- showExpr expr = showExpr' False expr where
--     add x = "(" ++ x ++ ")"

--     showExpr' :: Bool -> RegExp -> String
--     showExpr' _ (Epsilon) = ""
--     showExpr' _ (Lit x) = show x
--     showExpr' high (Cat l r) = 
--         let result = (showExpr' True l) ++ "" ++ (showExpr' True r)
--         in if high then add result else result
--     showExpr' high (Alt l r) = 
--         let result = (showExpr' True l) ++ "+" ++ (showExpr' True r)
--         in if high then add result else result
--     showExpr' _ (Star x) = (showExpr' True x) ++ "*"

-- instance Show RegExp where 
--     show = showExpr

--------------------------------------------------------------------------------------


import Data.List hiding (union)

data Reg = Epsilon | 
           Literal Char | 
           Or Reg Reg | 
           Then Reg Reg |
           Star Reg
           deriving Eq

literals :: Reg -> [Char]
literals Epsilon = []
literals (Literal ch) = [ch]
literals (Or r1 r2) = literals r1 ++ literals r2
literals (Then r1 r2) = literals r1 ++ literals r2
literals (Star r) = literals r

printReg :: Reg -> [Char]
printReg Epsilon = "ε"
printReg (Literal ch) = [ch]
printReg (Or r1 r2) = "(" ++ printReg r1 ++ "+" ++ printReg r2 ++ ")"
printReg (Then r1 r2) = "(" ++ printReg r1 ++ printReg r2 ++ ")"
printReg (Star r) = "(" ++ printReg r ++ ")*"

instance Show Reg where
    show = printReg

-- NFA 

data Nfa a = NFA (Set a) 
                (Set (Move a)) 
                a 
                (Set a)

printNFA :: Show a => Nfa a -> IO()
printNFA (NFA states moves start final)
    = putStr ("Stavy:\n" ++ (showSet states) ++ "\n\n" ++ 
    "Kroky:\n" ++ showMoves moves ++ "\n" ++
    "Pocatecni:\n" ++ (show start) ++ "\n\n" ++
    "Prijimajici:\n" ++ (showSet final) ++ "\n")

showMoves :: Show a => Set (Move a) -> String
showMoves (Set1 []) = ""
showMoves (Set1 (x:xs)) = (show x) ++ "\n" ++ showMoves (Set1 xs)

data Move a = Move a Char a | 
              EpsilonMove a a 
              deriving (Eq,Ord,Show)

-----------------------------------------------------------------

toNfa :: Reg -> Nfa Int

toNfa (Literal c) = NFA
                    (makeSet [0 .. 1])
                    (getSet (Move 0 c 1))
                    0
                    (getSet 1)

toNfa Epsilon = NFA
                (makeSet [0 .. 1])
                (getSet (EpsilonMove 0 1))
                0
                (getSet 1)

toNfa (Or r1 r2) = nfaOr (toNfa r1) (toNfa r2)
toNfa (Then r1 r2) = nfaThen (toNfa r1) (toNfa r2)
toNfa (Star r) = nfaStar (toNfa r)

nfaOr :: Nfa Int -> Nfa Int -> Nfa Int
nfaOr    (NFA states1 moves1 start1 finish1)
        (NFA states2 moves2 start2 finish2)
        = NFA 
            (states1' `union` states2' `union` newstates)
            (moves1' `union` moves2' `union` newmoves)
            0
            (getSet (m1+m2+1))
            where
                m1 = card states1
                m2 = card states2
                states1' = mapSet (renumber 1) states1
                states2' = mapSet (renumber (m1+1)) states2
                newstates = makeSet [0,(m1+m2+1)]
                moves1' = mapSet (renumber_move 1) moves1
                moves2' = mapSet (renumber_move (m1+1)) moves2
                newmoves = makeSet [EpsilonMove 0 1, EpsilonMove 0 (m1+1),
                                    EpsilonMove m1 (m1+m2+1),
                                    EpsilonMove (m1+m2) (m1+m2+1)]

nfaThen :: Nfa Int -> Nfa Int -> Nfa Int
nfaThen  (NFA states1 moves1 start1 finish1)
        (NFA states2 moves2 start2 finish2)
        = NFA 
            (states1' `union` states2' `union` newstates)
            (moves1' `union` moves2' `union` newmoves)
            0
            (getSet (m1+m2+1))
            where
                m1 = card states1
                m2 = card states2
                states1' = mapSet (renumber 1) states1
                states2' = mapSet (renumber (m1+1)) states2
                newstates = makeSet [0,(m1+m2+1)]
                moves1' = mapSet (renumber_move 1) moves1
                moves2' = mapSet (renumber_move (m1+1)) moves2
                newmoves = makeSet [EpsilonMove 0 1,
                                    EpsilonMove m1 (m1+1),
                                    EpsilonMove (m1+m2) (m1+m2+1)]

nfaStar :: Nfa Int -> Nfa Int
nfaStar  (NFA states1 moves1 start1 finish1)
        = NFA 
                (states1' `union` newstates)
                (moves1' `union` newmoves)
                0
                (getSet (m1+1))
                where
                m1 = card states1
                states1' = mapSet (renumber 1) states1
                newstates = makeSet [0,(m1+1)]
                moves1' = mapSet (renumber_move 1) moves1
                newmoves = makeSet [EpsilonMove 0 1,
                                    EpsilonMove m1 (m1+1),
                                    EpsilonMove 0 (m1+1),
                                    EpsilonMove m1 1]

renumber ::Int -> Int -> Int
renumber num = (+num)

renumber_move :: Int -> Move Int -> Move Int
renumber_move offset (Move x c y) = Move (x+offset) c (y+offset)
renumber_move offset (EpsilonMove x y) = EpsilonMove (x+offset) (y+offset)

----------------------------------------------------------------

reg :: Reg
reg = Star(Then (Or (Literal 'a') (Literal 'b')) (Literal 'c')) -- (((a+b)c))*


reg2 :: Reg
reg2 = Then (Then (Then (Literal 'b') (Literal 'a')) (Then (Literal 'b') (Literal 'b'))) (Literal 'a')

reg3 :: Reg
reg3 = Or (Literal 'a') (Literal 'b')

reg3Nfa :: Nfa Int
reg3Nfa = toNfa reg3


-- DFA
-----------------













































































------------------------------------------------------------------

splits :: [a] -> [([a],[a])]
splits str = [splitAt n str | n <- [0 .. length str]]


frontSplits :: [a] -> [([a],[a])]
frontSplits str = [splitAt n str | n <- [1 .. length str]]

matches :: Reg -> String -> Bool
matches Epsilon str = (str == "")
matches (Literal ch) str = (str == [ch])
matches (Or r1 r2) str = matches r1 str || matches r2 str
matches (Then r1 r2) str = 
    or [matches r1 s1 && matches r2 s2 |(s1,s2) <- splits str]
matches (Star r) str = matches Epsilon str ||
    or [matches r s1 && matches (Star r) s2 |(s1,s2) <- frontSplits str]

--------------------------------------------------------------------

data Set a = Set1 [a]


empty :: Set a
empty = Set1 []


getSet :: a -> Set a
getSet x = Set1 [x]

memSet :: Ord a => Set a -> a -> Bool
memSet (Set1 []) y = False
memSet (Set1 (x:xs)) y 
    | x<y  = memSet (Set1 xs) y
    | x == y = True
    | otherwise = False

union :: Ord a => Set a -> Set a -> Set a
union (Set1 xs) (Set1 ys) = Set1 (uni xs ys)

uni :: Ord a => [a] -> [a] -> [a]
uni [] ys = ys
uni xs [] = xs
uni (x:xs) (y:ys) 
    | x<y = x : uni xs (y:ys)
    | x == y = x : uni xs ys
    | otherwise = y : uni (x:xs) (ys)

inter :: Ord a => Set a -> Set a -> Set a
inter (Set1 xs) (Set1 ys) = Set1 (int xs ys)

int :: Ord a => [a] -> [a] -> [a]
int [] ys = []
int xs [] = []
int (x:xs) (y:ys) 
    | x<y = int xs (y:ys)
    | x == y = x : int xs ys
    | otherwise = int (x:xs) (ys)

diff :: Ord a => Set a -> Set a -> Set a
diff (Set1 xs) (Set1 ys) = Set1 (dif xs ys)

dif :: Ord a => [a] -> [a] -> [a]
dif [] ys = []
dif xs [] = xs
dif (x:xs) (y:ys) 
    | x<y = x : dif xs (y:ys)
    | x == y = dif xs ys
    | otherwise = dif (x:xs) ys

showSet :: Show a => Set a -> String
showSet (Set1 xs) = show xs

subSet :: Ord a => Set a -> Set a -> Bool
subSet (Set1 xs) (Set1 ys) = subS xs ys 

subS :: Ord a => [a] -> [a] -> Bool
subS [] ys  = True
subS xs [] = False
subS (x:xs) (y:ys)
    | x<y = False
    | x == y = subS xs ys
    | x>y = subS (x:xs) ys

eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (Set1 xs) (Set1 ys) = (xs ==  ys)

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f (Set1 xs) = makeSet (map f xs)

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet f (Set1 xs) = Set1 (filter f xs)

foldSet :: (a -> a -> a) -> a -> Set a -> a
foldSet f x (Set1 xs) = (foldr f x xs)

makeSet :: Ord a => [a] -> Set a
makeSet = Set1. remDups. sort where
            remDups [] = []
            remDups [x] = [x]
            remDups (x:y:xs)
                | x<y = x : remDups (y:xs)
                | otherwise = remDups (y:xs)

card :: Set a -> Int
card (Set1 xs) = length xs

flatten :: Set a -> [a]
flatten (Set1 xs) = xs

setlimit :: Eq a => (Set a -> Set a) -> Set a -> Set a
setlimit f s 
    | eqSet s next = s
    | otherwise = setlimit f next
    where 
        next = f s


-------------


fold_l :: (Set a -> Char-> Set a) -> Set a -> String -> Set a
fold_l f r [] = r
fold_l f r (c:cs) = fold_l f (f r c) cs

onemove :: Ord a => Nfa a -> Char -> Set a -> Set a
onemove (NFA states moves start final) c x
    = makeSet [s | t <- flatten x, (Move z d s) <- flatten moves, d == c, z == t]

closure :: Ord a => Nfa a -> Set a -> Set a
closure (NFA states moves start final)
    = setlimit add
        where
        add stateset = union stateset (makeSet accessible) 
                        where
                        accessible
                            = [s | t <- flatten stateset, EpsilonMove y s <- flatten moves, y == t]

trans :: Ord a => Nfa a -> String -> Set a
trans mach str = fold_l step startset str
                    where
                        step set ch = onetrans mach ch set
                        startset = closure mach (getSet (startstate mach)) 


onetrans :: Ord a => Nfa a -> Char -> Set a -> Set a
onetrans mach c x = closure mach (onemove mach c x)

startstate :: Nfa a -> a
startstate (NFA states moves start final) = start

machine :: Nfa Int
machine = NFA (makeSet [0 .. 3])
            (makeSet [Move 0 'a' 0,
                        Move 0 'a' 1,
                        Move 0 'b' 0,
                        Move 1 'b' 2,
                        Move 2 'b' 3])
            0
            (getSet 3)