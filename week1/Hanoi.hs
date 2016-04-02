module Hanoi where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _    = []
hanoi n t1 t2 t3 = hanoi (n-1) t1 t3 t2 ++ [(t1, t2)] ++ hanoi (n-1) t3 t2 t1
