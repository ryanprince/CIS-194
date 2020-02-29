module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage m =
  let parseMessage' ws = case ws of
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
parse log = map parseMessage (lines log)
