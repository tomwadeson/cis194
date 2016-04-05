{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List (tails)
import qualified Data.Map.Strict as M

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
histogram = show . map (\xs -> map (\x -> if x `elem` xs then '*' else ' ') [0..9]) . mkRows 0 . foldr (\x acc -> M.insertWith (+) x 1 acc) M.empty
  where
    mkRows n hist
      | n <= 9    = (M.keys . M.filter (>= n) $ hist) : mkRows (n+1) hist 
      | otherwise = []
