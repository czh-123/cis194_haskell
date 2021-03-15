

fun1:: [Integer] -> Integer
fun1  xs= foldr (*) 1 (map (\x -> x-2) ( filter (even) xs)) 


fun2::Integer -> Integer
fun2 = sum
     . filter even
     . takeWhile (/=1)
     . iterate (\n-> if even n then n `div` 2 else 3*n + 1)



data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
        deriving (Show,Eq)

foldTree :: Eq a => [a] -> Tree a
foldTree xs = foldr (balancedInsert start) Leaf xs  {-balan 接受start后变为接受两个参数的函数-}
  where start = floor (logBase 2 $ fromIntegral(length xs)::Double)

getTreeHeight:: Tree a -> Integer   {-低效-}
getTreeHeight Leaf = 0
getTreeHeight (Node n left root right) = max ( getTreeHeight left) ( getTreeHeight right) + 1

balancedInsert :: Eq a=> Integer -> a -> Tree a -> Tree a
balancedInsert _ x (Node n left y right)
                | getTreeHeight left == getTreeHeight right = Node n left y (balancedInsert (n-1) x right)
                | otherwise = Node n (balancedInsert (n-1) x left ) y right
balancedInsert start x _ = Node start Leaf x Leaf            



xor::[Bool] -> Bool
xor xs = odd $  foldr  (\n acc -> if n == True then  acc+1 else acc ) 0  xs {-左右折叠的参数 左折叠累计在左 右折叠累计在右 -}



map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\n acc -> f n :acc) []


--exercise3
myFoldl::(a->b->a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse  base  )

--exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ [1..n] \\ sieve
  where sieve = map (\(i, j) -> i + j + 2*i*j)
                . filter (\(i, j) -> i + j + 2*i*j <= n)
                $ cartProd [1..n] [1..n]

-- Return all possible pairs
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys



