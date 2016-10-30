{-# LANGUAGE FlexibleInstances #-}

f1 :: a -> a -> a
f1 x y = x

f2 :: a -> a -> a
f2 x y = y

f3 :: a -> a
f3 x = x

f5 :: a -> b -> a
f5 a _ = a

data Foo = F Int
         | G Char
  deriving (Eq, Ord, Show)

class Listable a where
    toList :: a -> [Int]

instance Listable Int where
    toList x = [x]

instance Listable Bool where
    toList True  = [1]
    toList False = [0]

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty        = []
  toList (Node x l r) = toList l ++ [x] ++ toList r
