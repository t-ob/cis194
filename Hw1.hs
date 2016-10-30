-- Exercise 1
toDigits    :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigitsRev n
    | n <= 0    = []
    | otherwise = n `mod` 10 : toDigitsRev(n `div` 10)

toDigits n = reverse(toDigitsRev(n))

-- Exercise 2
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x:(y:zs)) = (x : (2*y : doubleEveryOtherRev(zs)))

doubleEveryOther xs = reverse(doubleEveryOtherRev(reverse(xs)))

-- Exercise 3
sumDigits :: [Integer] -> Integer

sumDigits [] = 0
sumDigits (x:xs) = (x `div` 10) + (x `mod` 10) + sumDigits(xs)

-- Exercise 4
validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigits(n))) `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

-- hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- hanoi 0 _ _ _ = []
-- hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)

hanoi :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ _ = []
hanoi n a b c d
    | n `mod` 2 == 0 = (hanoi (n `div` 2) a c b d) ++ (hanoi ((n - 1) `div` 2) a d b c) ++ [(a, b)] ++ (hanoi ((n - 1) `div` 2) d b a c) ++ (hanoi (n `div` 2) c b a d)
    | otherwise      = (hanoi (n `div` 2) a c b d) ++ (hanoi (n `div` 2) a d b c) ++ [(a, b)] ++ (hanoi (n `div` 2) d b a c) ++ (hanoi (n `div` 2) c b a d)
