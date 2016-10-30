{-# OPTIONS_GHC -Wall #-}
module Hw4 where

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldr (\x a -> a * (x - 2)) 1 $ filter even xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

collatz :: Integer -> Integer
collatz n
    | even n    = n `div` 2
    | otherwise = 3*n + 1

fun2' :: Integer -> Integer
fun2' x = sum $ filter even $ takeWhile (>1) $ iterate collatz x

-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

xor :: [Bool] -> Bool
xor = foldr1 (/=)

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x ys -> (f x) : ys) [] xs

-- Exercise 4
-- Sieve of Sundaram

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2*x + 1 | x <- [1..n], j <- [1..n], i <- [1..j], i + j + 2*i*j <= n, x /= i + j + 2*i*j]
