module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single e _) = e
tag (Append e _ _) = e

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
ls1 +++ ls2 = (Append (mappend (tag ls1) (tag ls2)) ls1 ls2)


-- Exercise 2
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a

indexJ _ Empty     = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single s a) | i == 0 = Just a
indexJ i (Single s a) | i /= 0 = Nothing
indexJ i (Append s _ _) | i >= getSize (size s) = Nothing
indexJ i (Append s jl1 _) | i < getSize (size (tag jl1)) = indexJ i jl1
indexJ i (Append s jl1 jl2) = indexJ (i - getSize (size (tag jl1))) jl2


dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ i jl | i <= 0 = jl
dropJ _ Empty = Empty
dropJ 0 s@(Single _ _) = s
dropJ _ s@(Single _ _) = Empty
dropJ i (Append s _ _) | i >= getSize (size s) = Empty
dropJ i (Append s jl1 jl2) | i <= getSize (size (tag jl1)) = (dropJ i jl1) +++ jl2
dropJ i (Append s jl1 jl2) = dropJ (i - getSize (size (tag jl1))) jl2
