{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1a --> parse single LogMessage
parseStringList :: [String] -> LogMessage
parseStringList ("E" : errorNum : time : rest) = LogMessage (Error (read errorNum :: Int)) (read time :: Int) (unwords rest)
parseStringList ("I" : time : rest)            = LogMessage Info (read time :: Int) (unwords rest)
parseStringList ("W" : time : rest)            = LogMessage Warning (read time :: Int) (unwords rest)
parseStringList s                              = Unknown (unwords s)

parseMessage :: String -> LogMessage
parseMessage s = parseStringList (words s)

-- Exercise 1b --> parse full log file
parseLines :: [String] -> [LogMessage]
parseLines (firstLog : restLogs) = (parseMessage firstLog) : (parseLines restLogs)
parseLines [] = []

parse :: String -> [LogMessage]
parse logStr = parseLines (lines logStr)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert _ (Node _ (Unknown _) _) = Leaf
insert (Unknown _) mtree        = mtree
insert lm Leaf                  = Node Leaf lm Leaf
insert lm@(LogMessage _ insertTime _) mtree@(Node lTree nm@(LogMessage _ nodeTime _) rTree)
    | insertTime < nodeTime = Node (insert lm lTree) nm rTree
    | insertTime > nodeTime = Node lTree nm (insert lm rTree)
    | otherwise             = mtree

-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (firstLM : rest) = insert firstLM (build rest)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lTree lm rTree) = (inOrder lTree) ++ [lm] ++ (inOrder rTree)

-- Exercise 5
whatWentWrongSorted :: [LogMessage] -> [String]
whatWentWrongSorted []                          = []
whatWentWrongSorted ((LogMessage (Error errorNum) _ messageString) : rest)
    | errorNum > 50 = [messageString] ++ (whatWentWrongSorted rest)
    | otherwise     = whatWentWrongSorted rest
whatWentWrongSorted (_ : rest) = whatWentWrongSorted rest

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lm = (whatWentWrongSorted . inOrder . build) lm

