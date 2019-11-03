-- 1: Chess - http://behalek.cs.vsb.cz/wiki/index.php/FP_Homework_1/cs
import Data.Char

type Result = [String]

pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

getIndexByChar :: Char -> Int
getIndexByChar 'a' = 1
getIndexByChar 'b' = 2
getIndexByChar 'c' = 3
getIndexByChar 'd' = 4
getIndexByChar 'e' = 5
getIndexByChar 'f' = 6
getIndexByChar 'g' = 7
getIndexByChar 'h' = 8


getChessSymbol :: Int -> (Int, Int) -> [String] -> Char
--getChessSymbol _ (8,8) _ = '.'
getChessSymbol index (row, column) (x:xs) | index >= 4 = '.'
                                          | (digitToInt(x !! 2), getIndexByChar(x !! 1)) == (row, column) = x !! 0
                                          | otherwise = getChessSymbol (index + 1) (row, column) xs

generateLine :: Int -> [String] -> String 
generateLine index (x:xs) = let lineNumber = show index
                                symbols = map(\i -> getChessSymbol 0 (index, i) (x:xs)) [1..8]
                            in lineNumber ++ symbols

chess :: [String] -> [String] -> Result
chess (x:xs) (y:ys) = let bottomLine = [' '] ++ ['a'..'h']
                          innerLines = map(\i -> generateLine i (x:xs)) (reverse [1..8])
                      in innerLines ++ [bottomLine]
