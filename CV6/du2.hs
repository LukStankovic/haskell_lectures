-- 2: TickTackToe - http://behalek.cs.vsb.cz/wiki/index.php/FP_Homework_1/cs
import Data.Char

type Result = [String]

pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

getSymbol :: Int -> (Int, Int) -> [(Int, Int)] -> Char
getSymbol index (row, column) x | index >= length x = ' '
                            | (x !! index) == (row, column) = if index `mod` 2 == 0 then 'X' else 'O'
                            | otherwise = getSymbol (index + 1) (row, column) x

getInnerLine :: Int -> Int -> [(Int, Int)] -> String
getInnerLine n width x = let verticalLine = '|'
                             symbols = map(\i -> getSymbol 0 (n, i) x) [1..width]
                        in [verticalLine] ++ symbols ++ [verticalLine]

ticktack::(Int,Int) -> [(Int,Int)] -> Result
ticktack (width, height) (x:xs) = let topAndBottomLine = replicate (width + 2) '-'
                                      innerLines = map(\i -> getInnerLine i width (x:xs)) (reverse [1..height])
                                  in [topAndBottomLine] ++ innerLines ++ [topAndBottomLine]


linesWithoutCircle :: Result -> Int
linesWithoutCircle [] = 0
linesWithoutCircle (x:xs) | (countLetters x 'O') > 0 = 0 + linesWithoutCircle xs
                          | otherwise = 1 + linesWithoutCircle xs
                          
getElementAt :: [[Char]] -> (Int, Int) -> Char
getElementAt matrix (x, y) = (matrix !! y) !! x

fourInRow :: Result -> Int -> Int -> Int -> Int -> String
fourInRow matrix width height x y = map (getElementAt matrix) positions
        where positions = [(x, y), (if (x + 1) <= width then (x + 1) else 0, y), (if (x + 2) <= width then (x + 2) else 0, y), (if (x + 3) <= width then (x + 3) else 0, y)]

fourInColumn :: Result -> Int -> Int -> Int -> Int -> String
fourInColumn matrix width height x y = map (getElementAt matrix) positions
        where positions = [(x, y), (x, (if (y + 1) <= height then (y + 1) else 0)), (x, (if (y + 2) <= height then (y + 2) else 0)), (x, (if (y + 3) <= height then (y + 3) else 0))]

fourInDiagonalDown :: Result -> Int -> Int -> Int -> Int -> String
fourInDiagonalDown matrix width height x y = map (getElementAt matrix) positions
        where positions = [(x, y), (if (x + 1) <= width then (x + 1) else 0, (if (y + 1) <= height then (y + 1) else 0)), (if (x + 2) <= width then (x + 2) else 0, (if (y + 2) <= height then (y + 2) else 0)), (if (x + 3) <= width then (x + 3) else 0, (if (y + 3) <= height then (y + 3) else 0))]

fourInDiagonalUp :: Result -> Int -> Int -> Int -> Int -> String
fourInDiagonalUp matrix width height x y = map (getElementAt matrix) positions
        where positions = [(x, y), (if (x + 1) <= width then (x + 1) else 0, (if (y - 1) > 0 then (y - 1) else 0)), (if (x + 2) <= width then (x + 2) else 0, (if (y - 2) > 0 then (y - 2) else 0)), (if (x + 3) <= width then (x + 3) else 0, (if (y - 3) > 0 then (y - 3) else 0))]
       
countLetters :: String -> Char -> Int
countLetters str c = length $ filter (== c) str

isWinnerByXY :: Result -> (Int, Int) -> (Int, Int) -> Char -> Bool
isWinnerByXY m (width, height) (x, y) symbol | or [countLetters strRow symbol == 4, countLetters strColumn symbol == 4, countLetters strDiagonalDown symbol == 4, countLetters strDiagonalUp symbol == 4] = True
                                             | otherwise = False
                                             where strRow = fourInRow m width height x y
                                                   strColumn = fourInColumn m width height x y
                                                   strDiagonalDown = fourInDiagonalDown m width height x y
                                                   strDiagonalUp = fourInDiagonalUp m width height x y

checkWinnerXY :: Result -> (Int, Int) -> Int -> Char -> Bool
checkWinnerXY matrix (width, height) n symbol | or (map(\i -> isWinnerByXY matrix (width, height) (n, i) symbol) [1..width]) = True
                                              | otherwise = False  

checkWinner :: Result -> (Int, Int) -> Char -> Bool
checkWinner matrix (width, height) symbol | (or (map (\i -> checkWinnerXY matrix (width, height) i symbol) (reverse [1..height]))) = True
                                          | otherwise = False

winner :: (Int,Int) -> [(Int,Int)] -> Bool
winner (width, height) (x:xs) | checkWinner (ticktack (width, height) (x:xs)) (width, height) 'X'  = True
                              | checkWinner (ticktack (width, height) (x:xs)) (width, height) 'O'  = True
                              | otherwise = False



-- winner (8,8) [(1,1),(8,8),(2,2),(3,3),(4,2),(3,2)]
-- winner (8,8) [(1,1),(8,8),(2,2),(3,3),(4,2),(3,2), (3,1), (3,4),(4,4),(3,5)] - doprava row
-- winner (8,8) [(1,1),(8,8),(2,2),(6,6),(3,3),(3,2), (4,4), (3,4),(4,4),(3,5)] - diagonala nahoru
-- winner (8,8) [(1,1),(4,4),(2,3),(3,5), (6,3), (2,6), (1,4),(1,7)] -- diagonala dolu O
-- winner (8,8) [(1,1),(8,8),(2,1), (3,3), (3, 1), (4, 4), (4, 1)] -- column