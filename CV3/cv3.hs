import Data.Char

divisors :: Int -> [Int]
divisors n = tmp n where
    tmp 0 = []
    tmp y | n `mod` y == 0 = y: tmp(y - 1)
          | otherwise = tmp (y - 1) 


divisors' :: Int -> [Int]
divisors' n = filter (\x -> n `mod` x == 0) [1..n]

divisors'' :: Int -> [Int]
divisors'' n = [x| x <- [1..n], n `mod` x == 0] 


zipThem :: [a] -> [b] -> [(a,b)]
zipThem [] _ = []
zipThem _ [] = []
zipThem (x:xs) (y:ys) = (x,y) : zipThem xs ys

dotProduct :: [a] -> [b] -> [(a,b)]
dotProduct [] _ = []
dotProduct (x:xs) y = tmp y ++ dotProduct xs y where 
    tmp [] = []
    tmp (b:bs) = (x,b) : tmp bs

dotProduct' :: [a] -> [b] -> [(a,b)]
dotProduct' a b = zip (concat (map (replicate (length b)) a)) (concat(replicate(length a) b))

dotProduct'' :: [a] -> [b] -> [(a,b)]
dotProduct'' x y = [(a,b)| a <- x, b <- y]

allToUpper :: String -> String
allToUpper x = map toUpper x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let lp = filter (< x) xs 
                       rp = filter (>= x) xs
                   in quicksort lp ++ [x] ++ quicksort rp
