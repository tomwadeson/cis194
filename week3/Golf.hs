{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List (tails)

skips :: [a] -> [[a]]
skips = skips' 1
  where
    skips' n xs
      | n >= length xs  = []
      | otherwise       = (map fst . filter (\(_, i) -> i `mod` n == 0) $ zip xs [1..]) : skips' (n+1) xs

localMaxima :: [Integer] -> [Integer]
localMaxima = concatMap lm . filter (\x -> length x == 3) . map (take 3) . tails
  where
    lm [a, b, c]
      | b > a && b > c = [b]
      | otherwise      = []

histogram :: [Integer] -> String
histogram = undefined
