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
  Append (Size 2)
         (Single (Size 1) "Hello")
         (Single (Size 1) "World")

n =
  Append (Size 3)
         (Single (Size 1) "Greeting:")
         m
