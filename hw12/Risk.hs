{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List

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

------------------------------------------------------------
-- Risk

type Army = Int

data BattleField = BattleField { attackers :: Army, defenders :: Army }

instance Show BattleField where
        show bf = "Result :\nAttackers:\t" ++ show (attackers bf) ++ "\nDefenders:\t" ++ show (defenders bf)

getPlayers:: BattleField -> (Army, Army)
getPlayers bf = (att, def) where
    att = if attackers bf >= 4 then 3
                               else attackers bf - 1
    def = if defenders bf >= 2 then 2
                               else defenders bf

tally:: [Int] -> (Army, Army) -> (Army, Army) 
tally rands (nAtts, _) = loop (sort atts, sort defs) (0, 0) where
    atts = take nAtts rands
    defs = drop nAtts rands
    loop ([], _) result = result 
    loop (_, []) result = result
    loop (as, ds) (a, d) = loop (tail as, tail ds) (if head as > head ds then (a - 1, d) else (a, d - 1))

deduct:: BattleField -> (Army, Army) -> BattleField
deduct bf ps = BattleField (attackers bf + fst ps) (defenders bf + snd ps)

battle:: BattleField -> Rand StdGen BattleField
battle bf = replicateM (uncurry (+) ps) die >>= \rands ->
            return (deduct bf (tally (map unDV rands) ps)) where
                ps = getPlayers bf

invade:: BattleField -> Rand StdGen BattleField
invade bf = battle bf >>= \bf' ->
                if attackers bf' < 2 || defenders bf' <= 0
                    then return bf'
                    else invade bf'

success:: BattleField -> Bool
success = (== 0) . defenders

(|>) :: (b -> a) -> (a -> c) -> b -> c
(f |> g) x = g (f x)

successProb:: BattleField -> Rand StdGen Double
successProb = invade 
            |> replicateM 1000
            |> liftM (filter success)
            |> liftM (\xs -> fromIntegral (length xs) 
                            / 1000.0)

main:: IO()
main = do
        b <- evalRandIO (successProb (BattleField 4 10))
        print b
