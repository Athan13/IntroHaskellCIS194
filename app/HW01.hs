toDigits :: Integer -> [Integer]
toDigits n
    | n < 0     = []
    | n < 10    = [n]
    | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n < 0     = []
    | n < 10    = [n]
    | otherwise = [mod n 10] ++ toDigitsRev (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:rest) = x : 2*y : doubleEveryOther rest
doubleEveryOther rest = rest

sumDigits :: [Integer] -> Integer
sumDigits ns = sum (map (\n -> sum (toDigits n)) ns)

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigitsRev) n `mod` 10 == 0


type Peg = String
type Move = (Peg, Peg)
-- Integer is number of disks, pegs are names (a, b, c), moves list is moves in order to solve problem
hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi3 n a b c
    | n <= 0    = []
    | otherwise = (hanoi3 (n - 1) a c b) ++ [(a, c)] ++ (hanoi3 (n - 1) b a c)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
    | n <= 0    = []
    | otherwise = (hanoi4 (prev_tri) a c d b) ++ (hanoi3 (n - prev_tri) a c d) ++ (hanoi4 (prev_tri) b a c d)
    where 
        prev_tri = (2*n + 1 - round ((sqrt . fromIntegral) (1 + 8*n))) `div` 2