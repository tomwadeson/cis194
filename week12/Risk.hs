{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = rollFor bf >>= \rolls -> return $ runBattle bf rolls

runBattle :: Battlefield -> [(DieValue, DieValue)] -> Battlefield
runBattle (Battlefield att def) rolls = Battlefield att' def'
  where
    (att', def') = foldl (\(att', def') (a, d) -> if a > d then (att', def'-1) else (att'-1, def'))
                     (att, def) rolls

rollFor :: Battlefield -> Rand StdGen [(DieValue, DieValue)]
rollFor bf = let (attArmy, defArmy) = units bf
                 attackerRolls      = dice attArmy
                 defenderRolls      = dice defArmy
             in  attackerRolls >>= \ar ->
                 defenderRolls >>= \dr -> return $ zip (sort ar) (sort dr)

units :: Battlefield -> (Army, Army)
units (Battlefield att def) =
  let att' = if att > 1 then min (att - 1) 3 else 1
      def' = min def 2
  in  (att', def')

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = battle bf >>= \bf' ->
  if defenders bf' == 0 || attackers bf' < 2 then return bf' else invade bf'
