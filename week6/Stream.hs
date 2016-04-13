{-# OPTIONS_GHC -Wall #-}

module Stream where

data Stream a = Cons a (Stream a)
  deriving Eq

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs
