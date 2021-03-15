module Golf where


each :: Int -> [a] -> [a]
each n lst = [lst !! i| i <- [n-1, n-1+n..length lst -1]]
-- 但不确定是否好   因为用到了!!
skips :: [a] -> [[a]]
skips lst = [each i lst| i <- [1..length lst]]


localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (_:[]) = []
localMaxima (a:b:c:xs)
        | b > a && b > c  = b : localMaxima (b:c:xs)
        | otherwise = localMaxima xs (b:c:xs)

extract ::Int ->Int ->[Integer] -> [Integer]
extract k n = take k . drop (n-1)


histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
    where c = count xs
          m = maximum c

line :: [Int] -> Int ->String
line xs n = [if i >= n then '*' else ' '| i <- xs]

count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (== n) xs) [0..9]                                                                                                                                                                                                                                                                                                    