-- http://behalek.cs.vsb.cz/wiki/index.php/FP_Laboratory_7

type Pic = [String]

pic :: Pic
pic = [ "....#....",
        "...###...",
        "..#.#.#..",
        ".#..#..#.",
        "....#....",
        "....#....",
        "....#####"]

img = [ "....*....",
        "...#*#...",
        "..#.*.#..",
        ".#..*..#.",
        "..*****..",
        ".........",
        "........."]

pp :: Pic -> IO ()
pp x = putStr (concat (map (++"\n") x))


-- ultra složité :D 
flipH' :: Pic -> Pic
flipH' (pic:pics) = (reverse pics) ++ [pic]  

flipH :: Pic -> Pic
flipH = reverse

flipV :: Pic -> Pic
flipV = map reverse

flipV' :: Pic -> Pic
flipV' xs = [reverse x | x <- xs]

above :: Pic -> Pic -> Pic
--above a b = a ++ b
above = (++)

-- zip projde všechny seznamy, map jeden
sideBySide :: Pic -> Pic -> Pic
sideBySide = zipWith (++)

--sideBySide' :: Pic -> Pic -> Pic
--sideBySide' x y = [x++y] (x,y)<- zip xs ys

flipRow :: String -> Pic
flipRow xs = [ [x] | x<-xs ]

rotateR :: Pic -> Pic
rotateR [x] = flipRow x
rotateR (x:xs) = (rotateR xs) `sideBySide` (flipRow x)

rotateR' :: Pic -> Pic
rotateR' xss = foldl1 sideBySide [[[x]|x<-xs ] | xs <- (reverse xss)]

rotateL :: Pic -> Pic
rotateL xss = foldl1 sideBySide [[[x]|x <- reverse xs ] | xs <- xss]

zoom' :: Int -> Pic -> Pic
zoom' n xss = concat(map (replicate n) [concat [replicate n x | x <- xs] |xs <- xss])

--pěkněji
zoom :: Int -> Pic -> Pic
zoom n xss = concat([replicate n a | a <- [concat [replicate n x | x <- xs] |xs <- xss]])


-- cvičení 5 - http://behalek.cs.vsb.cz/wiki/index.php/FP_Laboratory_5
oddList :: Int -> Int -> [Int]
oddList a b = [x | x  <- [a..b ], odd x]

countThem :: String -> [(Char, Int)]
countThem xs = let u = unique  xs ""
               in  [(x, length(filter (==x) xs)) | x <- u] 

unique :: String -> String  ->  String
unique [] ys = reverse ys
unique (x:xs) ys | x `elem` ys = unique xs ys
              | otherwise = unique xs (x:ys)


isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..ceiling(sqrt (fromIntegral n)::Double)], n `mod` x == 0]

goldbach :: Int-> [(Int, Int)]
goldbach n = let primes = [x | x<-[1..(n `div` 2)], isPrime x] 
             in [(x, n -x) | x <- primes, isPrime(n - x)]

goldbachList :: Int -> Int-> Int -> [(Int, Int)]
goldbachList a b limit = filter (\(x,_) -> x>limit) [head (goldbach x) | x<-[a..b], even x]

combinations :: Int -> String -> [String]
combinations 1 xs = [[x] | x <- xs]
combinations n (x:xs) | n == length xs = [xs]
                      | otherwise = [[x] ++ y| y <- combinations (n-1) xs] ++ (combinations n xs)


not' :: Bool -> Bool
not' True = False
not' False = True

infixl 5 `not'`

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

infixl 4 `and'`

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

infixl 3 `or'`


nand' :: Bool -> Bool -> Bool
nand' x y = not(and' x y) 

infixl 4 `nand'`

xor' :: Bool -> Bool -> Bool
xor' x y =  x/=y

infixl 3 `xor'`

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

infixl 2 `impl'`

equ' :: Bool -> Bool -> Bool
equ' x y = x == y

infixl 7 `equ'`

--table :: (Bool -> Bool -> Bool) -> IO ()


tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = putStr(concat [show x ++ " -> " ++ show(f x) ++ "\n" | x <- allValues n]) where
    allValues 1 = [[True], [False]]
    allValues n = [x:y| x <- [True, False], y <- allValues (n-1) ]