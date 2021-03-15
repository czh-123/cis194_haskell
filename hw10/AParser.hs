{- CIS 194 HW 10
   due Monday, 1 April
-}
-- {-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }




-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------



--ex1
first :: (a->b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)
  
-- Parser f 
{-
data Maybe' a = Nothing' 
                | Just' a  deriving (Show) 

instance Functor Maybe' where
    fmap _ Nothing' = Nothing'
    fmap f (Just' a) = Just' (f a)
-}
{-
newtype Maybe' b a = Maybe' {getMaybe:: (a,b)}

instance Functor (Maybe' b) where
      fmap f (Maybe' (a,b)) = Maybe' (f a,b)
-}


instance Functor Parser where
    fmap f (Parser pf) = Parser (\s->fmap (first f) (pf s))
            -- Parser (fmap (first.f) pf ) wrong
            -- Parser (fmap (first f) . rp)
{-
instance Functor Parser where
    fmap f (Parser g) = Parser (\s -> first f <$> g s)
-}
      -- Parser pf   模式匹配  
      --pf的类型是String -> Maybe (a, String)
    

--ex2 
instance Applicative Parser where
    pure a = Parser f
      where f str = Just (a,str)  
    -- !!!!   这里实际上定义了 函数 f
    
    p1 <*> p2 = Parser f
          where f str =  case runParser p1 str of
                  Nothing           -> Nothing
                  Just (fRes,strRes) -> 
                          first fRes <$> runParser p2 strRes
              -- Just (fRes,strRes)   fRes :: a->b

--ex3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ ::Parser()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

-- const () <$> abParser

intPair ::Parser ([Integer])
intPair =  (\a _ c -> a:c:[]) <$> posInt <*> char ' ' <*> posInt
-- !!!!!!!!!!!!!!!!

--ex4 

instance Alternative Parser where
    empty =   Parser (\_ -> Nothing)

    -- empty <|> r = r  为什么要去? 
    {-
    p1 <|> p2 = Parser f
              where f str = case runParser p1 str of
                        Just (a,b) -> Just (a,b)
                        Nothing    -> runParser p2 str  
        -- warning warning: [-Woverlapping-patterns]
    -}

#define __TEST1__ 0 
#define __TEST2__ 1


#if __TEST1__
    (Parser p1) <|> (Parser p2) = Parser $ (\s -> p1 s <|> p2 s ) 
#endif

#if __TEST2__
    (Parser p1) <|> (Parser p2) = Parser $ (\s -> case p1 s of
                                                Nothing -> p2 s 
                                                otherwise -> p1 s)
#endif


-- test
a::Parser Char
a = char 'a' <|> char 'c'

--

--ex5

upperCase::Parser ()
upperCase = (\_ -> ()) <$> satisfy isUpper
--        const() <$> satisfy isUpper
intOrUppercase :: Parser ()
intOrUppercase = (\ _ -> ()) <$> posInt <|> upperCase
