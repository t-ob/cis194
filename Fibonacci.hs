{-# LANGUAGE FlexibleInstances #-}

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = map (\(a, b) -> a) $ iterate (\(a, b) -> (b, a + b)) (0, 1)

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show s = show $ take 20 $ streamToList s

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) s = Cons x (interleaveStreams s xs)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

-- Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n                 = Cons n (streamRepeat 0)
    negate (Cons a as)            = Cons (-a) (negate as)
    (+) (Cons a as) (Cons b bs)   = Cons (a+b) (as + bs)
    (*) (Cons a as) r@(Cons b bs) = Cons (a*b) ((streamMap (*a) bs) + as*r)

instance Fractional (Stream Integer) where
    (/) l@(Cons a as) r@(Cons b bs) = Cons (a `div` b) (streamMap (\x -> x `div` b) (as - (l/r) * bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer
  deriving (Show, Eq)

instance Num Matrix where
    (*) (Matrix a b c d) (Matrix a' b' c' d') = Matrix (a*a' + b*c') (a*b' + b*d') (c*a' + d*c') (c*b' + d*d')

fib4 :: Integer -> Integer
fib4 n = case (Matrix 1 1 1 0)^n of
           Matrix _ b _ _ -> b
