fact1 0 = 1
fact1 n = n * fact1(n-1)

fact2 n | n == 0 = 1
        | otherwise = n * fact2(n - 1)

fact3 n = if n==0 then 1 else n * fact3 (n-1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

fib2 :: Int -> Int
fib2 0 = 0
fib2 1 = 1
fib2 n = tmp n 0 1

tmp 1 a b = b
tmp n a b = tmp(n-1) b (a+b)

leapYear :: Int -> Bool
leapYear y
    | isDivBy 400 = True
    | isDivBy 100 = False
    | isDivBy 4   = True
    | otherwise   = False
    where isDivBy a = mod y a == 0

max2 :: Int -> Int -> Int
max2 a b
    | (a > b) = a
    | otherwise = b

max3 :: Int -> Int -> Int -> Int
max3 a b c
    | (a >= b && a >= c) = a
    | (b >= a && b >= c) = b
    | otherwise = c

euclid :: Int -> Int -> Int
euclid 0 b = b
euclid a b = euclid (b `mod` a) a

euclid2 :: Int -> Int -> Int
euclid2 a b | a > b = euclid2(a-b) b
            | a < b = euclid2 a (b-a)
            | a == b = a

isPrime :: Int -> Bool
-- isPrime n 
 --       | n < 2 = False
 --       | n == 2 = True
 --       | mod n 2 == 0 = False
        
isPrime 1 = False
isPrime n = isPrimeTest n (ceiling (sqrt (fromIntegral(n - 1))))

isPrimeTest n 1 = True
isPrimeTest n x | n `mod` x == 0 = False
                | otherwise = isPrimeTest n (x-1)


length2 :: [a] -> Int
length2 [] = 0
length2 (_:xs) = 1 + length2 xs

sumIt :: [Int] -> Int
sumIt [] = 0
sumIt (x:xs) = x + sumIt xs

getHead :: [a] -> a
getHead (x:_) = x

getLast :: [a] -> a
getLast [x] = x
getLast (_:xs) = getLast xs

isElement :: Eq a => a -> [a] -> Bool
isElement a [] = False
isElement a (x:xs) = if x == a then True else isElement a xs


combine :: [a] -> [a] -> [a]
combine xs     []     = xs
combine []     ys     = ys
combine (x:xs) (y:ys) = x : y : combine xs ys


combine2 :: [a] -> [a] -> [a]
combine2 [] y = y
combine2 (x:xs) y = x : combine2 xs y

amax2 :: [Int] -> Int
amax2 [x] = x
amax2 (x:y:z) | x > y = amax2 (x:z)
              | otherwise = amax2 (y:z)


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]


reverse'' :: [a] -> [a]
reverse'' x = tmp x []
    where tmp [] ys = ys
          tmp (x:xs) ys = tmp xs (x:ys)


take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x: take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (_:xs) = drop' (n-1) xs