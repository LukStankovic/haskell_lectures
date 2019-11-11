-- module Stack (Stack, empty, isEmpty, push, top, pop) where

--     empty :: Stack a
--     isEmpty :: Stack a -> Bool
--     push :: a -> Stack a -> Stack a
--     top :: Stack a -> a
--     pop :: Stack a -> (a,Stack a)
    
--     newtype Stack a = StackImpl [a]
--     empty = StackImpl []
--     isEmpty (StackImpl s) = null s
--     push x (StackImpl s) = StackImpl (x:s)
--     top (StackImpl s) = head s
--     pop (StackImpl (s:ss)) = (s,StackImpl ss)

-- import Data.Maybe

-- data Stack a = Stack [a] deriving Show

-- empty :: Stack a 
-- empty = Stack []

-- push :: a -> Stack a -> Stack a
-- push x (Stack xs) = Stack (x:xs)

-- pop :: Stack a -> (Maybe a, Stack a)
-- pop (Stack []) = (Nothing, Stack [])
-- pop (Stack (x:xs)) = (Just x, Stack xs)


-- -- isEmpty :: Stack a -> Bool
-- -- isEmpty _ = True
-- -- isEmpty x | null x = True
-- --           | otherwise = False

-- test1 :: Stack Int
-- test1 = push 4 $ push 3 $ push 10 empty



type Stack a = [a]
 
create :: Stack a
create = []
 
add :: a -> Stack a -> Stack a
add = (:)
 
removeNotGet :: Stack a -> Stack a
removeNotGet []     = error "Prázdné"
removeNotGet (x:xs) = xs


removeAndGet :: Stack a -> a
removeAndGet []     = error "Prázdné"
removeAndGet (x:xs) = x

remove :: Stack a -> (a, Stack a)
remove []     = error "Stack empty"
remove (x:xs) = (x,xs)

empty :: Stack a -> Bool
empty = null
 
peek :: Stack a -> a
peek []    = error "Prázdné"
peek (x:_) = x

-- let a = add 99 create
-- let a' = add 88 a
-- let a'' = add 44 a'
