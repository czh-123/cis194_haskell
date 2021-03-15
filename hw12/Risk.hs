{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Applicative
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
{-
 newtype Rand g a = Rand (RandT g Identity a)
    deriving (Functor, Monad,
     MonadRandom, MonadSplit g) 

-- | Evaluate a random computation in the IO monad, using the random number
-- generator supplied by 'System.Random.getStdRandom'.
evalRandIO :: Rand StdGen a -> IO a
evalRandIO (Rand (RandT x)) = getStdRandom (runIdentity . runStateT x)

-}




dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


battle :: Battlefield ->Rand StdGen Battlefield 
battle b@(Battlefield atk def) = 
      do 
          let attackArmy = max 0 (atk-1)
              defendArmy = max 2 def
          aRolls <- sortedRolls attackArmy
          dRolls <- sortedRolls defendArmy 
          return $ foldl scrap b (zipWith (,) aRolls dRolls)



sortedRolls ::Int -> Rand StdGen [DieValue]
sortedRolls n = fmap (sortBy (flip compare)) $ dice n


scrap :: Battlefield -> (DieValue,DieValue) -> Battlefield
scrap (Battlefield a d) (attack,defense)
  | attack > defense = Battlefield a (d-1)
  | otherwise = Battlefield (a-1) d

--ex3

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d) 
  | a < 2 || d == 0 = return b
  | otherwise = do
        result <- battle b
        invade result

--ex4
successProb ::Battlefield->Rand StdGen Double
successProb b@(Battlefield a d) = do
    battles <- replicateM 1000 (invade b)
    return $ fromIntegral 
      (length $ filter ((==0) . defenders) battles) /1000.0



