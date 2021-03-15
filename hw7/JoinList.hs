{-# LANGUAGE FlexibleInstances
           , TypeSynonymInstances
   #-}

module JoinList where

import Data.Monoid
import Scrabble
import Buffer
import Editor
--import Scrabble
import Sized

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
        deriving (Eq,Show)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty


-- ex2
indexJ :: (Sized b , Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n (Single _ a) 
        | n == 0 = Just a
        | otherwise = Nothing
indexJ n (Append m l1 l2)
        | n < 0 || n > size0 = Nothing
        | n < size1          = indexJ n l1
        | otherwise          = indexJ (n - size1) l2
         where size0 = getSize . size $ m
               size1 = getSize . size . tag $ l1
indexJ _ _ = Nothing


--drop 丢弃
dropJ ::(Sized b,Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n l1@(Single _ _)
  | n <= 0 = l1
dropJ n l@(Append m l1 l2)
  | n >= size0 = Empty
  | n >= size1 = dropJ (n-size1) l2
  | n > 0 = dropJ n l1 +++ l2
  | otherwise  = l
    where size0 = getSize . size $ m
          size1 = getSize . size . tag $ l1
dropJ _ _ = Empty


takeJ ::(Sized b,Monoid b) => Int ->JoinList b a ->JoinList b a
takeJ n l1@(Single _ _)
        | n <= 0 = l1
takeJ n root@(Append count left right)
        | n >= rootSize = Empty
        | n >= leftSize = left +++ takeJ (n-leftSize) right
        | n >= 0 = takeJ n left
         where leftSize = getSize . size . tag $ left
               rootSize = getSize . size  $ count
takeJ _ _ = Empty


--ex3 + Scrabble
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str


--ex4
instance Buffer (JoinList (Score,Size) String ) where
        toString Empty = "" 
        toString (Single _ str) = str
        toString (Append _ left right) = toString left ++ toString right

        fromString =  foldr (+++) Empty . map lineToJl . lines
                where lineToJl = (\str->Single (scoreString str, (Size 1)) str)


        line   = indexJ

        replaceLine i str buf = takeJ i buf +++ fromString str +++ dropJ (i+1) buf

        numLines  = getSize . snd.tag

        value   = getScore . fst.tag






-- test

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

a = Append (Size 3)
      (Append (Size 2)
        (Single (Size 1) "hi")
        (Single (Size 1) "bye")
      )
     (Single (Size 1) "tschau")

b = Single (Size 1) "blub"

c = Append (Size 2)
      (Single (Size 1) "hi")
      (Single (Size 1) "bye")

greeting :: String
greeting = unlines
    [ "This buffer is for notes you don't want to save, and for"
    , "evaluation of steam valve coefficients."
    , "To load a different file, type the character L followed"
    , "by the name of the file."
    ]

-- Run the editor with the greeting as the initial buffer.

main = runEditor editor initialBuffer
    where initialBuffer = (fromString greeting :: JoinList (Score, Size) String)