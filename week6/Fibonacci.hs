{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n | n >= 2 = fib (n-1) + fib (n-2)
fib _ = 0

fibs1 :: [Integer]
fibs1 = map fib [1..]
