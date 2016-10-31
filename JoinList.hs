module JoinList where

import Data.Monoid

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
