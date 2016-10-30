data IntList = Empty | Cons Int IntList
  deriving Show

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList f (Cons x xs)
              | (f x)     = Cons x (filterIntList f xs)
              | otherwise = (filterIntList f xs)

even :: Int -> Bool
even x = x `mod` 2 == 0

reduceIntList :: (Int -> Int -> Int) -> Int -> IntList -> Int
reduceIntList _ acc Empty       = acc
reduceIntList f acc (Cons x xs) = reduceIntList f (f acc x) xs

data List t = E | C t (List t)
  deriving Show

lst1 :: List Int
lst2 :: List Char
lst3 :: List Bool

lst1 = C 3 (C 5 (C 7 E))
lst2 = C 'x' (C 'y' (C 'z' E))
lst3 = C True (C False E)

filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList f (C x lst)
           | (f x)     = C x (filterList f lst)
           | otherwise = (filterList f lst)

mapList :: (a -> b) -> List a -> List b
mapList _ E = E
mapList f (C x xs) = C (f x) (mapList f xs)

reduceList :: (a -> a -> a) -> a -> List a -> a
reduceList _ acc E = acc
reduceList f acc (C x xs) = reduceList f (f acc x) xs

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
