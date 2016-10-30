biggestInt, smallestInt :: Int
biggestInt  = maxBound
smallestInt = minBound

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise      = 3*n + 1

p :: (Int, Char)
p = (3, 'x')

myIterate :: (Integer -> Integer) -> Integer -> [Integer]
myIterate f n = [(f n)] ++ myIterate f (f n)

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n = n : collatz (hailstone n)

intListLength :: [Integer] -> Integer
intListLength []      = 0
intListLength (x: xs) = 1 + intListLength xs
