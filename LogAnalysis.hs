{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Parse an individual log line.
parseMessage :: String -> LogMessage
--parseMessage line = Unknown (unwords(words(line)))
parseMessage line = case words(line) of
                      ("I" : (timestampStr : infoWords))               -> LogMessage Info (read timestampStr::Int) (unwords infoWords)
                      ("W" : (timestampStr : warningWords))            -> LogMessage Warning (read timestampStr:: Int) (unwords warningWords)
                      ("E" : (levelStr : (timestampStr : errorWords))) -> LogMessage (Error (read levelStr::Int)) (read timestampStr::Int) (unwords errorWords)
                      _ -> Unknown line

-- Parse a string of logs, separated by newlines, into log messages.
parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- Insert a log message into an ordered MessageTree.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert l Leaf = Node Leaf l Leaf
insert l@(LogMessage _ logMessageTimestamp _) (Node leftTree m@(LogMessage _ nodeLogMessageTimestamp _) rightTree)
    | logMessageTimestamp < nodeLogMessageTimestamp = Node (insert l leftTree) m rightTree
    | otherwise                                     = Node leftTree m (insert l rightTree)
insert _ tree = tree

-- Build a MessageTree from a list of log messages.
build :: [LogMessage] -> MessageTree
build []                  = Leaf
build (logMessage : rest) = insert logMessage (build rest)

-- Traverse a MessageTree in order and return a list of log messages
-- in chronological order.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree logMessage rightTree) = (inOrder leftTree) ++ [logMessage] ++ (inOrder rightTree)

-- Return only error messages from a list of log messages.
errorMessages :: [LogMessage] -> [LogMessage]
errorMessages [] = []
errorMessages (l@(LogMessage (Error severity) _ _) : rest)
    | severity >= 50 = l : errorMessages(rest)
errorMessages (_ : rest) = errorMessages(rest)

-- Return string content from log messages.
content :: [LogMessage] -> [String]
content []                               = []
content ((LogMessage _ _ string) : rest) = string : (content rest)
content ((Unknown string) : rest)        = string : (content rest)

-- Return log messages from severe errors in chronological order.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = content . errorMessages . inOrder . build
