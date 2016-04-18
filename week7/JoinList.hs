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
indexJ _ Empty = Nothing
indexJ index (Single _ x) 
  | index == 0 = Just x
  | otherwise  = Nothing
indexJ index (Append s l1 l2) 
  | index < 0 || index > size0 = Nothing
  | index < size1 = indexJ index l1
  | otherwise     = indexJ (index - size1) l2
  where
    size0 = getSize . size $ s
    size1 = getSize . size . tag $ l1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n l1@(Single _ _)
  | n <= 0 = l1
dropJ n l0@(Append m l1 l2)
  | n >= size0 = Empty
  | n >= size1 = dropJ (n-size1) l2
  | n > 0      = dropJ n l1 ++++ l2
  | otherwise  = l0
  where
    size0 = getSize . size $ m
    size1 = getSize . size . tag $ l1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _     = Empty
takeJ n l1@(Single _ _)
  | n >= 1 = l1
takeJ n l0@(Append m l1 l2)
  | n >= size0 = l0
  | n < size1  = takeJ n l1
  | n >= size1 = l1 ++++ takeJ (n - size1) l2
  where
    size0 = getSize . size $ m
    size1 = getSize . size . tag $ l1

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jToList :: JoinList m a -> [a]
jToList Empty            = []
jToList (Single _ x)     = [x]
jToList (Append _ l1 l2) = jToList l1 ++ jToList l2

p =
  Append (Size 2)
         (Single (Size 1) "Hello")
         (Single (Size 1) "World")

q =
  Append (Size 3)
         (Single (Size 1) "Greeting:")
         p
