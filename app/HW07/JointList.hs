module JointList where

import Sized
import Scrabble
import Buffer
import Data.Text (replace)

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- EXERCISE 1
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2
    
-- EXERCISE 2.1
(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0    = Just x
(x:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


tagSize :: (Sized m, Monoid m) => JoinList m a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized m, Monoid m) => Int -> JoinList m a -> Maybe a
indexJ _ Empty        = Nothing
indexJ _ (Single _ a) = Just a
indexJ index jl@(Append _ jlLeft jlRight)
    | index < 0              = Nothing
    | index >= tagSize jl    = Nothing
    | index < tagSize jlLeft = indexJ index jlLeft
    | otherwise              = indexJ (index - tagSize jlLeft) jlRight

-- EXERCISE 2.2
dropJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
dropJ n Empty             = Empty
dropJ n jl@(Single _ _)   = if n == 0 then jl else Empty
dropJ n jl@(Append m jlLeft jlRight)
    | n == tagSize jlLeft = jlRight
    | n > tagSize jlLeft  = dropJ (n - tagSize jlLeft) jlRight  
    | otherwise           = Append m (dropJ n jlLeft) jlRight

-- EXERCISE 2.3
takeJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
takeJ n Empty             = Empty
takeJ n jl@(Single _ _)   = if n == 0 then Empty else jl
takeJ n jl@(Append m jlLeft jlRight)
    | n == tagSize jlLeft = jlLeft
    | n < tagSize jlLeft  = takeJ n jlLeft  
    | otherwise           = Append m jlLeft (takeJ (n - tagSize jlLeft) jlRight)

-- EXERCISE 3 --> see Scrabble module
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

-- EXERCISE 4

instance Buffer (JoinList (Score, Size) String) where
    toString Empty                     = []
    toString (Single _ str)            = str
    toString (Append _ jlLeft jlRight) = toString jlLeft ++ toString jlRight

    fromString str = case length $ lines str of
                        1 -> Single (scoreString str, Size 1) str
                        n -> Append (scoreString str, Size n) 
                                    (fromString $ unlines $ take (n `div` 2) $ lines str)
                                    (fromString $ unlines $ drop (n `div` 2) $ lines str)

    line = indexJ

    replaceLine _ _ Empty = Empty
    replaceLine index newStr (Single m oldStr) = Single m (if index == 0 then newStr else oldStr)
    replaceLine index newStr (Append m jlLeft jlRight)
        | index < numLines jlLeft = Append m (replaceLine index newStr jlLeft) jlRight
        | otherwise = Append m jlLeft (replaceLine (index - numLines jlLeft) newStr jlRight) 

    numLines Empty = 0
    numLines (Single _ _) = 1
    numLines (Append (_, Size n) _ _) = n

    value Empty = 0
    value (Single (Score n, _) _)   = n
    value (Append (Score n, _) _ _) = n
