{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage m =
  let parseMessage' wordList = case wordList of
        ("I":time:ws)          -> LogMessage
                                    Info
                                    (read time :: Int)
                                    (unwords ws)
        ("W":time:ws)          -> LogMessage
                                    Warning
                                    (read time :: Int)
                                    (unwords ws)
        ("E":severity:time:ws) -> LogMessage
                                    (Error (read severity :: Int))
                                    (read time :: Int)
                                    (unwords ws)
        ws                     -> Unknown (unwords ws)
  in parseMessage' (words m)


parse :: String -> [LogMessage]
parse logData = map parseMessage (lines logData)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf  = Node Leaf logMessage Leaf
insert message@(LogMessage _ time _) (Node left nodeMessage@(LogMessage _ nodeTime _) right)
  | time < nodeTime = Node (insert message left) nodeMessage right
  | otherwise       = Node left nodeMessage (insert message right)
insert _ tree       = tree

-- Exercise 3
build :: [LogMessage] -> MessageTree
build messages = foldr insert Leaf messages

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
  Leaf                      -> []
  (Node left message right) -> (inOrder left) ++ (message : inOrder right)

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logData =
  let isCriticalError message = case message of
        LogMessage (Error severity) _ _ -> severity >= 50
        _                               -> False
      criticalErrors          = filter isCriticalError (inOrder (build logData))
  in map (\(LogMessage _ _ message) -> message) criticalErrors
