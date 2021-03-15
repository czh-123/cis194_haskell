{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log


parseMessage ::String -> LogMessage
parseMessage str = let wordList = words str 
                   in case wordList of
                       ("I":ts:msg) -> LogMessage Info (read ts ::Int)  (unwords msg)
                       ("W":ts:msg) -> LogMessage Warning (read ts::Int) ( unwords msg)
                       ("E":etime:ts:msg) -> LogMessage (Error (read etime ::Int)) (read ts::Int) (unwords msg)
                       _            -> Unknown (unwords wordList)

parse :: String -> [LogMessage]
parse = map parseMessage . lines


getTime :: LogMessage ->Int
getTime (LogMessage _ a _) = a
getTime (Unknown _) =  -100000

insert :: LogMessage -> MessageTree -> MessageTree  
insert (Unknown _) p = p
insert node Leaf = Node Leaf node Leaf
insert node@(LogMessage _ b _) (Node left root right)  -- 用_ b _ 优化
    |b < getTime root = Node (insert node left) root right --
    |b > getTime root = Node  left root (insert node right) --

insert2 :: MessageTree->LogMessage->MessageTree
insert2 a b = insert b a

build ::[LogMessage] -> MessageTree
build = foldr insert Leaf    -- foldr 

inOrder ::MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = inOrder left ++ [root] ++ inOrder right

judgeError :: LogMessage -> Bool
judgeError (LogMessage (Error a) _ _ )
            |a >= 50 = True
            |otherwise = False
judgeError _ = False

getErrorMessage::LogMessage->String 
getErrorMessage (LogMessage _ _ c) = c
getErrorMessage _ = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong  p = map (getErrorMessage )
                    (inOrder . build . filter (judgeError) $ p )









