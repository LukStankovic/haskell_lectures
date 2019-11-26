
import Control.Monad( liftM, liftM2 )
import Data.Set (Set)
import qualified Data.Set as S
import Data.List hiding (union)

data Reg = Empty
         | Epsilon 
         | Literal Char 
         | Or Reg Reg 
         | Then Reg Reg 
         | Star Reg       
           deriving Eq

data Move a = Move a Char a | Emove a a
            deriving (Eq,Ord,Show)

data Nfa a  = NFA (Set a) (Set (Move a)) a (Set a)
           deriving (Eq,Show)


pp :: [Char] -> IO ()
pp x = putStr (x)

printReg :: Reg -> [Char]
printReg Empty   = "_"
printReg Epsilon = "&"
printReg (Literal ch) = [ch]
printReg (Or r1 r2) = "(" ++ printReg r1 ++ "+" ++ printReg r2 ++ ")"
printReg (Then r1 r2) = "(" ++ printReg r1 ++ printReg r2 ++ ")"
printReg (Star r) = "(" ++ printReg r ++")*" 

instance Show Reg where
    show = printReg

literals :: Reg -> [Char]
literals Empty = []
literals Epsilon = []
literals (Literal ch) = [ch]
literals (Or r1 r2) = literals r1 ++ literals r2
literals (Then r1 r2) = literals r1 ++ literals r2
literals (Star r) = literals r


-- vypis pro nfa i dfa
print_automat :: Nfa Int -> IO()
print_automat (NFA states moves start finish) = pp (print_nfa (NFA states moves start finish))

--- NFA
print_nfa :: Nfa Int -> [Char]
print_nfa (NFA states moves start finish)
      = "Stavy:\t" ++ show_states (S.toList states) ++ "\n\n" ++
        "Kroky:\n" ++ (concat (map print_move (S.toList moves))) ++ "\n\n" ++
        "Pocatecni: " ++ show start ++ "\n" ++
        "Prijimajici: " ++ show_states (S.toList finish) ++ "\n"

show_states :: [Int] -> [Char]
show_states = concat . (map ((++" ") . show))

print_move :: Move Int -> [Char]
print_move (Move s1 c s2) = "\t " ++ show s1 ++ " ---- " ++ [c] ++ " ---> "
                            ++ show s2 ++ "\n"
print_move (Emove s1 s2) = "\t " ++ show s1 ++ " ---- & ---> " ++ show s2 ++ "\n"

 
reg_to_nfa :: Reg -> Nfa Int
reg_to_nfa Epsilon = NFA
                     (S.fromList [0 .. 1])
                     (S.singleton  (Emove 0 1))
                     0
                     (S.singleton  1)
reg_to_nfa (Literal c) = NFA
                     (S.fromList [0 .. 1])
                     (S.singleton  (Move 0 c 1))
                     0
                     (S.singleton  1)
reg_to_nfa (Or r1 r2) = or_move (reg_to_nfa r1) (reg_to_nfa r2)
reg_to_nfa (Then r1 r2) = then_move (reg_to_nfa r1) (reg_to_nfa r2)
reg_to_nfa (Star r) = star_move (reg_to_nfa r)

-- Jednotlivé operace - nebo, spojeni, iterace
or_move :: Nfa Int -> Nfa Int -> Nfa Int
or_move (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
    = NFA
      (S.unions [states1',states2',newstates])
      (S.unions [moves1' ,moves2' ,newmoves] )
      0
      (S.singleton (m1+m2+1))
    where
      m1 = S.size states1
      m2 = S.size states2
      states1' = S.map (renumber 1) states1
      states2' = S.map (renumber (m1+1)) states2
      newstates = S.fromList [0,(m1+m2+1)]
      moves1' = S.map (renumber_move 1) moves1
      moves2' = S.map (renumber_move (m1+1)) moves2
      newmoves = S.fromList [Emove 0 1, Emove 0 (m1+1),
                             Emove m1 (m1+m2+1), Emove (m1+m2) (m1+m2+1) ]

then_move :: Nfa Int -> Nfa Int -> Nfa Int
then_move (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
    = NFA
      (S.union states1 states2')
      (S.union moves1 moves2')
      start1
      finish2'
    where
      states2' = S.map (renumber k) states2
      moves2' = S.map (renumber_move k) moves2
      finish2' = S.map (renumber k) finish2
      k = S.size states1 - 1

star_move :: Nfa Int -> Nfa Int 
star_move (NFA states moves start finish)
    = NFA
      (S.union states' newstates)
      (S.union moves' newmoves)
      0
      (S.singleton (m+1))
    where
      m = S.size states
      states' = S.map (renumber 1) states
      newstates = S.fromList [ 0 , m+1 ]
      moves' = S.map (renumber_move 1) moves
      newmoves = S.fromList [Emove 0 1, Emove m 1, Emove 0 (m+1), 
                             Emove m (m+1)]


closure :: Ord a => Nfa a -> Set a -> Set a
closure (NFA states moves start term)
      = setlimit add
        where
        add stateset = S.union stateset (S.fromList accessible)
            where
              accessible
                  = [ s | x <- S.toList stateset , 
                          (Emove y s) <- S.toList moves ,
                          y==x ]


setlimit :: (Ord a) => (Set a -> Set a) -> Set a -> Set a
setlimit f s = let next = f s
                in if s==next 
                      then s 
                      else setlimit f next


onemove :: Ord a => Nfa a -> Char -> Set a -> Set a
onemove (NFA states moves start term) c x
      = S.fromList [ s | t <- S.toList x , 
                      Move z d s <- S.toList moves ,
                      z==t , c==d ]

onetrans :: Ord a => Nfa a -> Char -> Set a -> Set a
onetrans mach c x = closure mach (onemove mach c x)

startstate :: Nfa a -> a
startstate (NFA states moves start finish) = start

alphabet :: Nfa a -> [Char]
alphabet (NFA s moves st f) 
    = nub [ c | Move s c t <- S.toList moves ]



renumber :: Int -> Int -> Int
renumber n st = n + st

renumber_move :: Int -> Move Int -> Move Int
renumber_move k (Move s1 c s2)
        = Move (renumber k s1) c (renumber k s2)
renumber_move k (Emove s1 s2)
        = Emove (renumber k s1) (renumber k s2)


-- Převod na deterministicky

make_deterministic :: Nfa Int -> Nfa Int
make_deterministic = number . make_deter

number :: Nfa (Set Int) -> Nfa Int
number (NFA states moves start finish)
    = NFA states' moves' start' finish'
    where
    statelist = S.toList states
    lookup l a = look 0 l a
    look n [] a = error "prazdne"
    look n (b:y) a 
        | (b==a)      = n                 
        | otherwise   = look (n+1) y a    
    change = lookup statelist
    states' = S.map change states
    moves'  = S.map newmove moves
                where
                newmove (Move s c t) = Move (change s) c (change t) 
                newmove (Emove s t)  = Emove (change s) (change t)
    start' = change start
    finish' = S.map change finish


make_deter :: Nfa Int -> Nfa (Set Int)
make_deter mach = deterministic mach (alphabet mach)

deterministic :: Nfa Int -> [Char] -> Nfa (Set Int)
deterministic mach alpha 
    = nfa_limit (addstep mach alpha) startmach
        where
        startmach = NFA 
                    (S.singleton starter)
                    S.empty
                    starter
                    finish
        starter = closure mach (S.singleton start)
        finish 
            = if (term `S.intersection` starter) == S.empty   
            then S.empty                
            else S.singleton starter    
        (NFA sts mvs start term) = mach

addstep :: Nfa Int -> [Char] -> Nfa (Set Int) -> Nfa (Set Int)
addstep mach alpha dfa
    = add_aux mach alpha dfa (S.toList states)
    where
    (NFA states m s f) = dfa
    add_aux mach alpha dfa [] = dfa
    add_aux mach alpha dfa (st:rest) 
        = add_aux mach alpha (addmoves mach st alpha dfa) rest

-- proiteruje vsechny kroky
addmoves :: Nfa Int -> Set Int -> [Char] -> Nfa (Set Int) -> Nfa (Set Int)
addmoves mach x [] dfa    = dfa
addmoves mach x (c:r) dfa = addmoves mach x r (addmove mach x c dfa)

-- pridani noveho kroku
addmove :: Nfa Int -> Set Int -> Char -> Nfa (Set Int) -> Nfa (Set Int)
addmove mach x c (NFA states moves start finish)
    = NFA states' moves' start finish'
    where 
    states' = states `S.union` (S.singleton new)
    moves'  = moves  `S.union` (S.singleton (Move x c new))
    finish' 
        = if (S.empty /= (term `S.intersection` new))
            then finish `S.union` (S.singleton new)       
            else finish                   
    new = onetrans mach c x
    (NFA s m q term) = mach

nfa_limit :: Eq a => (Nfa a -> Nfa a) -> Nfa a -> Nfa a
nfa_limit f n 
    | (nfa_eq n next) = n         
    | otherwise = nfa_limit f next
                where
                next = f n
                nfa_eq (NFA s1 n1 st1 f1) (NFA s2 n2 st2 f2)
                    = s1 == s2 && n1 == n2 && st1 == st2 && f1 == f2



-- Testovací

regexp = Or (Then (Literal 'a') (Literal 'b')) (Then (Literal 'b') (Literal 'a'))
regexp2 = Or (Literal 'a') (Literal 'b')

nfa_regexp = reg_to_nfa regexp
nfa_regexp2 = reg_to_nfa regexp2

dfa_regexp = make_deterministic (reg_to_nfa regexp)
dfa_regexp2 = make_deterministic (reg_to_nfa regexp2)

-- print_automat (reg_to_nfa regexp) -- NFA
-- print_automat (make_deterministic (reg_to_nfa regexp2 )) - do DFA
