-------------- Regular Expression Data type -------------

data RegExp = Empty
           | Epsilon 
           | Literal Char 
           | Or RegExp RegExp 
           | Then RegExp RegExp 
           | Star RegExp  
           deriving Eq


-- instance Show RegExp where
--     show = printRegExp
  
printRegExp :: RegExp -> [Char]
printRegExp Empty   = "_"
printRegExp Epsilon = "@"
printRegExp (Literal ch) = [ch]
printRegExp (Or r1 r2) = "(" ++ printRegExp r1 ++ "|" ++ printRegExp r2 ++ ")"
printRegExp (Then r1 r2) = "(" ++ printRegExp r1 ++ printRegExp r2 ++ ")"
printRegExp (Star r) = "(" ++ printRegExp r ++")*"


----------------- MATCH REGULAR EXPRESSION FUNCTION -------------------------
splits :: [a] -> [([a],[a])]  -- napriklad [2,3] je [([], [2,3]), ([2],[3]),([2,3],[])]
splits st = [ splitAt n st | n <- [0 .. length st]]

frontSplits :: [a] -> [([a],[a])] -- stejne jako splits, ale preskoci prvni prvek - Epsilon
frontSplits st = [ splitAt n st | n <- [1 .. length st]]


matches :: RegExp -> String -> Bool
matches Empty st = False
matches Epsilon st = (st == "")
matches (Literal ch) st = (st == [ch])
matches (Or r1 r2) st = matches r1 st || matches r2 st
matches (Then r1 r2) st = or [matches r1 s1 && matches r2 s2 | (s1,s2) <- splits st]
matches (Star r) st = matches Epsilon st || or [matches r s1 && matches (Star r) s2 | (s1,s2) <- frontSplits st]



regMatch1 :: RegExp
regMatch1 = Star((Literal 'a') `Or` (Star((Literal 'b') `Then` (Literal 'c')))) -- (a|(bc)*)*

-- matches regMatch1 "@"

regMatch2 :: RegExp
regMatch2 = (Literal 'a') `Then` (Star((Literal 'b') `Or` (Literal 'a')) `Then` Star((Literal 'b') `Then` (Literal 'a'))) -- a((b|a)*(ba)*)

-- matches regMatch2 "abba"



---------------------------------
-- ************ MAIN ************
--------------------------------- 
recognizeMatch reg str = do
  putStrLn ""
  putStr "Regular expression: "
  putStrLn (printRegExp reg)
  putStr "String: "
  putStrLn (str ++ "\n")
  putStrLn "Substrings: "
  putStrLn "-----------------------------"
  loopStringWithRegExp reg (split ' ' str)
  
-- Examples
-- recognizeMatch regMatch2 "abba aa baa baa abbb abc"
-- recognizeMatch regMatch1 "@ ab aa ba abc"

loopString :: [String] -> IO ()
loopString [] = putStrLn ""
loopString (x:xs) = do
  putStrLn x
  loopString xs


loopStringWithRegExp :: RegExp -> [String] -> IO ()
loopStringWithRegExp reg [] = putStrLn ""
loopStringWithRegExp reg (x:xs) = do
  let match | matches reg x = "True" 
            | otherwise = "False"
  putStrLn (x ++ ": " ++ match)
  loopStringWithRegExp reg xs


split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s
