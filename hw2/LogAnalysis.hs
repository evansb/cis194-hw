{-# OPTIONS_GHC -Wall #-}

-- CIS194 Homework 2
-- By Evan Sebastian (evans@comp.nus.edu.sg)
-- http://www.seas.upenn.edu/~cis194/hw/02-ADTs.pdf

module LogAnalysis where

import Log

isNumeric:: String -> Bool
isNumeric s = case reads s :: [(Int, String)] of
                  [(_, "")] -> True
                  _         -> False

-- parseMessage construct a LogMessage from a String
parseMessage:: String -> LogMessage
parseMessage msg = (par . splitAt 2 . words) msg where
    par (["E", n] , xs) | isNumeric $ head xs = LogMessage (Error (read n::Int)) (read (head xs)::Int) (unwords $ tail xs)
    par (["I", n] , xs) | isNumeric n = LogMessage Info (read n::Int) (unwords xs)
    par (["W", n] , xs) | isNumeric n = LogMessage Warning (read n::Int) (unwords xs)
    par _ = Unknown msg

parse:: String -> [LogMessage]
parse = map parseMessage . lines

newNode:: LogMessage -> MessageTree
newNode a = Node Leaf a Leaf

instance Ord LogMessage where
    (LogMessage _ ts1 _) `compare` (LogMessage _ ts2 _) = ts1 `compare` ts2
    _ `compare` _ = error "Compare error"

-- insert add a LogMessage to the MessageTree, BST sorted
insert:: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = newNode message 
insert message (Node Leaf node rightChild) 
        |  message < node = Node (newNode message) node rightChild;
insert message (Node leftChild node Leaf)
        |  message > node = Node leftChild node (newNode message)
insert message (Node leftChild node rightChild)
        |  message < node = Node (insert message leftChild) node rightChild
        |  otherwise = Node leftChild node (insert message rightChild)

build:: [LogMessage] -> MessageTree
build = loop Leaf where
    loop tree [] = tree
    loop tree msgs = loop (insert (head msgs) tree) (tail msgs)

inOrder:: MessageTree -> [LogMessage]
inOrder = recurse [] where
    recurse xs Leaf = xs ++ []
    recurse xs (Node left node right) = xs ++ recurse xs left ++ [node] ++ recurse xs right

whatWentWrong:: [LogMessage] -> [String]
whatWentWrong = map messageOf . filter wrong where
    messageOf:: LogMessage -> String
    messageOf (LogMessage _ _ s) = s
    messageOf _ = error "No message, huh?"

    wrong:: LogMessage -> Bool
    wrong (LogMessage (Error s) _ _) = s > 50 
    wrong (LogMessage _ s _) = s > 50
    wrong _ = False
