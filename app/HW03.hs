import Data.List

-- Exercise 1 - Hopscotch
getEveryN :: Integer -> [a] -> [a]
getEveryN n = map snd . filter (\x -> mod (fst x) n == 0) . zip [1..]

skips :: [a] -> [[a]]
skips list = map (uncurry $ getEveryN) (zip [1..] (replicate (length list) list))

-- Exercise 2 - Local Maxima
getDiffs :: [Integer] -> [[Integer]]
getDiffs intList = map (\[x, y, z] -> [x, y - x, z - x]) $ transpose [(tail $ init intList), (init $ init intList), (tail $ tail intList)]

localMaxima :: [Integer] -> [Integer]
localMaxima = map head . filter (\[x, d1, d2] -> d1 < 0 && d2 < 0) . getDiffs

localMaxima1 :: [Integer] -> [Integer]  -- GitHub solution
localMaxima1 (a:b:c:xs)
    | (b > a) && (b > c) = b : localMaxima (b:c:xs)
    | otherwise          = localMaxima (b:c:xs)
localMaxima1 _ = []

-- Exercise 3 - Histogram
singleColumn :: Int -> (Int, Int) -> String
singleColumn max (i, count) = show i ++ "=" ++ (replicate count '*') ++ (replicate (max - count) ' ')

histogram :: [Int] -> String
histogram ns = let counts = map (\n -> length $ filter (==n) ns) [0..9]
                   max = maximum counts
            in unlines $ transpose $ map (reverse . singleColumn max) (zip [0..9] counts)