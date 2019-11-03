factorial :: Int -> Int
factorial n = product[1..n]

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

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

