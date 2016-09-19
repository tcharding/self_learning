{-# OPTIONS_GHC -Wall #-}

--module LogAnalysis where

import Log

{- parseMessage "E 2 562 help help"
    == LogMessage (Error 2) 562 "help help"

   parseMessage "I 29 la la la"
    == LogMessage Info 29 "la la la"

   parseMessage "This is not in the right format"
    == Unknown "This is not in the right format" -}

parseMessage :: String -> LogMessage
parseMessage s = go $ words s
-- blows up if message if t or l are not Ints
  where go ("I":t:xs) = LogMessage Info (read t) (unwords xs)
        go("E":l:t:xs) = LogMessage (Error (read l)) (read t) (unwords xs)
        go xs = Unknown (unwords xs)
  
parse :: String -> [LogMessage]
--parse s = foldr parseMessage [] $ lines s
parse s = go (lines s)
  where go [] = []
        go (x:xs) = parseMessage x : go xs

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert msg Leaf = Node Leaf msg Leaf  
insert msg@(LogMessage _ t _) (Node l (LogMessage _ u _) r)
  | t < u = insert msg l
  | otherwise = insert msg r    -- allows non-unique timestamps
insert (LogMessage _ _ _) (Node _ (Unknown _) _) = error "required for exhaustive pattern match"

build :: [LogMessage] -> MessageTree
build msgs = go msgs Leaf
  where go [] t = t
        go (m:ms) t = go ms (insert m t)

inOrder :: MessageTree -> [LogMessage]
inOrder (Node l msg r) = inOrder l ++ [msg] ++ inOrder r
inOrder _ = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map msgOnly (filter p msgs)
  where p (LogMessage (Error lvl) _ _)
          | lvl >= 50 = True
        p _ = False

msgOnly :: LogMessage -> String
msgOnly (LogMessage _ _ msg) = msg
msgOnly (Unknown msg) = msg
