{-# LANGUAGE ViewPatterns #-}
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise =  toDigits (n `div`10) ++ [n `mod` 10]

toDigitsRev ::Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = n `mod` 10 : toDigits (n`div`10)


subDigits :: [Integer] -> Integer
subDigits [] = 0
subDigits (x:xs) = x `mod` 10 + x`div`10 + subDigits xs 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) oneTwo . reverse
    where oneTwo = 1 : 2 : oneTwo 


validate :: Integer -> Bool
validate n   
    | (subDigits.doubleEveryOther.toDigits $n ) `mod` 10 == 0 = True
    | otherwise = False
    