import Data.List

-- EXERCISE 1
-- Given functions:
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

-- Simplified
fun1' :: [Integer] -> Integer
fun1' = product . map (\n -> n - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (\n -> n /= 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

-- EXERCISE 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)
 
minDepth :: Tree a -> Integer
minDepth (Node depth Leaf _ Leaf)  = depth
minDepth (Node depth left _ Leaf)  = depth
minDepth (Node depth Leaf _ right) = depth
minDepth (Node depth left _ right) = min (minDepth left) (minDepth right)
minDepth _ = 0

insertInTree :: a -> Tree a -> Tree a
insertInTree insert (Node depth Leaf a Leaf)  = Node depth (Node (depth + 1) Leaf insert Leaf) a Leaf
insertInTree insert (Node depth left a Leaf)  = Node depth left a (Node (depth + 1) Leaf insert Leaf)
insertInTree insert (Node depth Leaf a right) = Node depth (Node (depth + 1) Leaf insert Leaf) a right
insertInTree insert (Node depth left a right) = insertInTree insert (if minDepth left <= minDepth right then left else left)
insertInTree insert _ = Node 0 Leaf insert Leaf

foldTree :: [a] -> Tree a
foldTree = foldr (insertInTree) Leaf

-- EXERCISE 3
xor :: [Bool] -> Bool
xor = foldr (\x y -> if x then (not y) else y) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\new curr -> (f new) : curr) []

-- EXERCISE 3 (optional)
myFoldL :: (a -> b -> a) -> a -> [b] -> a
myFoldL f base xs = foldr (\new curr -> curr `f` new) base $ reverse xs

-- EXERCISE 4
cartProd :: [Integer] -> [Integer] -> [(Integer, Integer)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys, x <= y]

getComposites :: Integer -> [Integer]
getComposites n = map (\(x, y) -> x + y + 2 * x * y) $ cartProd [1..n] [1..n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\m -> 2 * m + 1) $ filter (\m -> not $ m `elem` getComposites n) [1..n]
