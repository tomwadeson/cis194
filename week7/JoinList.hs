module JoinList where

import Data.Monoid ((<>))
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(++++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(++++) x y = Append (tag x <> tag y) x y

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty          = Nothing
indexJ n x@(Single _ y) = if (n == (getSize . size . tag $ x)) then (Just y) else Nothing
indexJ n x@(Append _ l r)
  | n <  (getSize . size . tag $ x) = indexJ n l
  | n >  (getSize . size . tag $ x) = indexJ n r
