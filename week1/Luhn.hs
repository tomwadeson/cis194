module Luhn where

import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits = map (toInteger . digitToInt) . show

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = snd . foldr (\x (double, acc) -> 
  let double' = not double 
  in  if double then (double', (x*2) : acc) 
                else (double', x : acc)) (False, [])

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate n = check `mod` 10 == 0
  where
    check = sumDigits . doubleEveryOther . toDigits $ n
