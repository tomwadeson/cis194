module Scrabble where

import Data.Monoid
import Data.Char (toUpper)

newtype Score = Score { getScore :: Int }
  deriving (Eq, Show)

instance Monoid Score where
  mempty = Score 0
  mappend (Score s1) (Score s2) = Score (s1 + s2)

score :: Char -> Score
score 'A' = Score 1
score 'B' = Score 3
score 'C' = Score 3
score 'D' = Score 2
score 'E' = Score 1
score 'F' = Score 4
score 'G' = Score 2
score 'H' = Score 4
score 'I' = Score 1
score 'J' = Score 8
score 'K' = Score 5
score 'L' = Score 1
score 'M' = Score 3
score 'N' = Score 1
score 'O' = Score 1
score 'P' = Score 3
score 'Q' = Score 10
score 'R' = Score 1
score 'S' = Score 1
score 'T' = Score 1
score 'U' = Score 1
score 'V' = Score 3
score 'W' = Score 4
score 'X' = Score 8
score 'Y' = Score 4
score 'Z' = Score 10
score _   = Score 0

scoreString :: String -> Score
scoreString = mconcat . map (score . toUpper)
