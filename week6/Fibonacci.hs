{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n | n >= 2 = fib (n-1) + fib (n-2)
fib _ = 0

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = fib 0 1
  where
    fib x y = x : fib y (x+y)

data Stream a = Cons a (Stream a)
  deriving Eq

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = undefined

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))

x :: Stream Integer
x = Cons 0 (Cons 1 $ streamRepeat 0)

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0

  negate = streamMap (* (-1))

  (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)

  (*) (Cons x xs) y'@(Cons y ys) = Cons (x * y) $ streamMap (*x) ys + (xs * y')

data Matrix = Matrix Integer Integer Integer Integer
  deriving (Eq, Show)

instance Num Matrix where
  (*) (Matrix a b c d) (Matrix e f g h) = let a' = a * e + b * g 
                                              b' = a * f + b * h
                                              c' = c * e + d * g
                                              d' = c * f + d * h
                                          in  Matrix a' b' c' d'

fib4 :: Integer -> Integer
fib4 n = let (Matrix _ _ x _) = matrix^n in x
  where
    matrix = Matrix 1 1 1 0
