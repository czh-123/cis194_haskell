
module Scrabble where


import Data.Monoid
import Data.Char

newtype Score  = Score Int
    deriving (Eq,Ord,Show)

getScore::Score -> Int
getScore (Score a) = a

score :: Char -> Score
score c
    | c' `elem` "aeioulnstr" = Score 1
    | c' `elem` "dg" = Score 2
    | c' `elem` "bcmp" = Score 3
    | c' `elem` "fhvwy" = Score 4
    | c' `elem` "k" = Score 5
    | c' `elem` "jx" = Score 8
    | c' `elem` "qz" = Score 10
    | otherwise = Score 0
    where c' = toLower c

instance Semigroup Score where
    (Score x) <> (Score y) = Score (x+y)

instance Monoid Score where
    mempty = Score 0
    (Score x) `mappend` (Score y) = Score (x+y)


scoreString :: String -> Score
scoreString [] = Score 0
scoreString (x:xs) = (score x) <> scoreString xs 



