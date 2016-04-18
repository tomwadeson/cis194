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

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jToList :: JoinList m a -> [a]
jToList Empty            = []
jToList (Single _ x)     = [x]
jToList (Append _ l1 l2) = jToList l1 ++ jToList l2

m =
  Append (Size 3)
         (Single (Size 1) "Hello")
         (Single (Size 1) "World")

n =
  Append (Size 4)
         (Single (Size 1) "Greeting:")
         m
