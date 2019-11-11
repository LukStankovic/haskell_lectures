data Queue a = Q [a] [a] deriving Show
 
emptyQueue :: Queue a
emptyQueue = Q [] []
 
push :: Queue a -> a -> Queue a
push (Q input output) item = Q (item:input) output
 
pop :: Queue a -> (Maybe a, Queue a)
pop (Q input (item:output)) = (Just item, Q input output)
pop (Q [] []) = (Nothing, Q [] [])
pop (Q input []) = pop (Q [] (reverse input))
 
isEmpty :: Queue a -> Bool
isEmpty (Q [] []) = True
isEmpty _ = False


-- let q = foldl push emptyQueue [2..10]