{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JoinList where

import Data.Monoid
import Buffer
import Sized
import Scrabble
import Editor


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
dropJ _ s@(Single _ _) = Empty
dropJ i (Append _ jl1 jl2) | i <= getSize (size (tag jl1)) = (dropJ i jl1) +++ jl2
dropJ i (Append _ jl1 jl2) = dropJ (i - getSize (size (tag jl1))) jl2


takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ i _ | i <= 0 = Empty
takeJ _ Empty = Empty
takeJ i s@(Single _ _) = s
takeJ i (Append _ jl1 _) | i <= getSize (size (tag jl1)) = takeJ i jl1
takeJ i (Append _ jl1 jl2) = jl1 +++ (takeJ (i - getSize (size (tag jl1))) jl2)


-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


-- Exercise 4
-- jlFold :: c -> (a -> b -> c) -> (a -> c -> c -> c) -> JoinList a b -> c
-- jlFold a _ _ Empty = a
-- jlFold a f _ (Single m x) = f m x
-- jlFold a f g (Append m jl1 jl2) = g m (jlFold a f g jl1) (jlFold a f g jl2)

instance Buffer (JoinList (Score, Size) String) where
    toString Empty              = ""
    toString (Single _ s)       = s
    toString (Append _ jl1 jl2) = toString jl1 ++ toString jl2

    fromString s = foldr (+++) Empty $ map (\l -> Single (scoreString l, Size 1) l) $ lines s

    line = indexJ

    replaceLine i s jl
        | (i >=) . getSize . size .  snd . tag $ jl = jl
        | otherwise                                 = takeJ i jl +++ fromString s +++ dropJ (i + 1) jl

    numLines = getSize . snd . tag

    value = getScore . fst . tag



welcomeBuffer = foldr (+++) Empty $ map (\s -> Single (scoreString s, Size 1) s)
        [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main = runEditor editor $ welcomeBuffer
