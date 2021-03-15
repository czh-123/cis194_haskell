

module Fibonacci where



fib ::Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)


fibs1 :: [Integer]
fibs1 = fmap fib [0..]

--exercise 2

fibs2 :: [Integer]
fibs2 = 0:1 :zipWith (+) fibs2 (tail fibs2)

fibo :: Integer -> Integer -> [Integer]
fibo a b = a : fibo b (a+b) 

fibs3 ::[Integer]
fibs3 =fibo 0 1 


--exercise 3

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList


streamToList :: Stream a  ->[a]
streamToList (Cons y c) = y : streamToList c


--exercise 4

streamRepeat ::a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap ::(a->b) -> Stream a -> Stream b
streamMap f (Cons a y) = Cons (f a) (streamMap f y)


streamFromSeed::(a->a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

--exercise 5

nats::Stream Integer
nats = streamFromSeed (+1) 0

ruler::Stream Integer
ruler = startRuler 0
--a(2*n+1) = 1; a(2*n) = 1 + a(n).
interleaveStreams ::Stream a -> Stream a -> Stream a
interleaveStreams (Cons y ys) zs = Cons y (interleaveStreams zs ys)

startRuler :: Integer ->Stream Integer
startRuler y = interleaveStreams (streamRepeat y) (startRuler (y+1))



