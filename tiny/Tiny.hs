module Tiny (
   Exp(..), Com(..), State(..),  -- exportovane konstruktory
   Prog, Store, Input, Output,   -- exportovane typy
   e, c, p                       -- exportovane funkce
)
where

data Exp = Add   Exp Exp      -- e + e
         | Sub   Exp Exp      -- e - e
         | Mul   Exp Exp      -- e * e
         | Div   Exp Exp      -- e / e
         | Mod   Exp Exp      -- e % e
         | Neg   Exp          -- -e
         | Equ   Exp Exp      -- e == e
         | Neq   Exp Exp      -- e != e
         | Lth   Exp Exp      -- e < e
         | Gth   Exp Exp      -- e > e
         | Leq   Exp Exp      -- e <= e
         | Geq   Exp Exp      -- e >= e
         | And   Exp Exp      -- e && e
         | Or    Exp Exp      -- e || e
         | Not   Exp          -- !e
         | Asgn  String Exp   -- id = e
         | Cond  Exp Exp Exp  -- e ? e : e
         | Read               -- read
         | Num   Int          -- num
         | Var   String       -- id

data Com = Eval  Exp          -- e;
         | If    Exp Com Com  -- if( e ) c else c
         | While Exp Com      -- while( e ) c
         | Do    Exp Com      -- do c while( e );
         | Write Exp          -- write e;
         | Seq   Com Com      -- c ; c
         | Skip               -- ;
         | For   Exp Exp Exp  -- for e e e

type Store  = String -> Int
type Input  = [Int]
type Output = [Int]
data State  = State {store::Store, input::Input, output::Output}

--------------------------------
-- semantika vyrazu
--------------------------------

e :: Exp -> State -> (Int, State)

evalBin :: (Int -> Int -> Int) -> Exp -> Exp -> State -> (Int, State)
evalBin op e1 e2 s = let (v1,s') = e e1 s 
                     in let (v2,s'') = e e2 s'
                        in ((op v1 v2), s'')

evalCond :: (Int -> Int -> Bool) -> Exp -> Exp -> State -> (Int, State)
evalCond op e1 e2 s = let (v1,s') = e e1 s 
                      in let (v2,s'') = e e2 s'
                         in (if op v1 v2 then 1 else 0, s'')

e (Add e1 e2) s = evalBin (+) e1 e2 s
e (Sub e1 e2) s = evalBin (-) e1 e2 s
e (Mul e1 e2) s = evalBin (*) e1 e2 s
e (Div e1 e2) s = evalBin div e1 e2 s
e (Mod e1 e2) s = evalBin mod e1 e2 s

e (Equ e1 e2) s = evalCond (==) e1 e2 s
e (Neq e1 e2) s = evalCond (/=) e1 e2 s
e (Lth e1 e2) s = evalCond (<)  e1 e2 s
e (Gth e1 e2) s = evalCond (>)  e1 e2 s
e (Leq e1 e2) s = evalCond (<=) e1 e2 s
e (Geq e1 e2) s = evalCond (>=) e1 e2 s

e (Asgn v e1) s = let (v1,(State st i o)) = e e1 s
                      s'' v' = if v'==v then v1
                               else st v'
                  in (v1, (State s'' i o))

e (Cond b e1 e2) s = let (v1,s') = e b s
                     in if v1 == 0 then e e2 s'
                                   else e e1 s'

e Read (State s (x:i) o) = (x, (State s i o))
e Read (State _ [] _) = error "Read: prazdny vstup"

e (Num x) s = (x, s)

e (Var v) (State s i o) = (s v, (State s i o))

e (And e1 e2) s = evalBin (*) e1 e2 s
e (Or e1 e2) s = evalBin (+) e1 e2 s
e (Not e1) s = let (v, s') = e e1 s
               in (if v == 0 then 1 else 0, s')
--------------------------------
-- semantika prikazu
--------------------------------

c :: Com -> State -> State

c (Eval e1)  s = let (_,s') = e e1 s
                 in s'

c (If b c1 c2) s = let (v1,s') = e b s
                   in if v1 == 0 then c c2 s'
                      else c c1 s'

c (While b c1) s = let (v1,s') = e b s
                   in if v1 == 0 then s'
                    else c (While b c1) (c c1 s')

c (Seq c1 c2) s = c c2 (c c1 s)

c (Write e1) s = let (v1, (State s' i o)) = e e1 s
                 in (State s' i (o++[v1]))

c Skip s = s

c (For a b c) s

--------------------------------
-- semantika programu
--------------------------------

type Prog = Com

p :: Prog -> Input -> Output

emptyStore :: Store
emptyStore id = error ("Store: Promenna "++id++" neni definovana")

initialState :: Input -> State
initialState inp = State {store=emptyStore, input=inp, output=[]}

p body inp = out 
             where State{output=out} = c body (initialState inp)

------------------------------------------------------------------------
