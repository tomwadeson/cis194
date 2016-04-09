{-# OPTIONS_GHC -Wall #-}

module Week4 where

fun1 :: [Integer] -> Integer
fun1 []       = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate 
  (\x -> if even x then x `div` 2 else 3 * x + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldl (flip insert) Leaf

mkNode :: a -> Integer -> Tree a
mkNode x h = Node h Leaf x Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf                    = mkNode x 0
insert x (Node h Leaf val right) = Node h (mkNode x (h+1)) val right
insert x (Node h left val Leaf)  = Node h left val (mkNode x (h+1))
insert x (Node h left val right)
  | maxHeight left <= maxHeight right = Node h (insert x left) val right
  | otherwise                         = Node h left val (insert x right)

maxHeight :: Tree a -> Integer
maxHeight Leaf                  = 0
maxHeight (Node h left _ right) = maximum [h, maxHeight left, maxHeight right]

xor :: [Bool] -> Bool
xor = foldl xor' False

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _        = False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f base xs = foldr (flip f) base $ reverse xs
