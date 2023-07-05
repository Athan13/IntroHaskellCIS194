{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Language.Haskell.TH (Lit(IntegerL))

-- EXERCISE 1
fib :: Integer -> Integer
fib n
    | n <= 1 = n
    | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- EXERCISE 2
fibs2 :: [Integer]
fibs2 = getFibs 0 1
    where
        getFibs :: Integer -> Integer -> [Integer]
        getFibs a b = a : getFibs b (a + b)

-- EXERCISE 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons first rest) = first : streamToList rest

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- EXERCISE 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons first rest) = Cons (f first) $ streamMap f rest

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed $ streamFromSeed f (f seed)

-- EXERCISE 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = streamMap (highestPower2 0 . (+ 1)) nats
    where
        highestPower2 :: Integer -> Integer -> Integer
        highestPower2 highest n
            | even n    = highestPower2 (highest + 1) (n `div` 2)
            | otherwise = highest

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x1 xrest) ys = Cons x1 (interleaveStreams ys xrest)

ruler' :: Stream Integer
ruler' = interleaver 0
    where
        interleaver :: Integer -> Stream Integer
        interleaver n = interleaveStreams (streamRepeat n) (interleaver (n + 1))

-- EXERCISE 6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger :: Integer -> Stream Integer
    fromInteger n = Cons n $ streamRepeat 0

    negate :: Stream Integer -> Stream Integer
    negate = streamMap (* (-1))

    (Cons a0 a') + (Cons b0 b') = Cons (a0 + b0) (a' + b')

    (Cons a0 a') * b@(Cons b0 b') = Cons (a0 * b0) (streamMap (*a0) b' + (a' * b))

instance Fractional (Stream Integer) where
    a@(Cons a0 a') / b@(Cons b0 b') = Cons (a0 `div` b0) (streamMap (* (1 `div` b0)) (a' - (a/b)*b'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- EXERCISE 7
data Matrix = Matrix Integer Integer Integer Integer

instance Show Matrix where
    show (Matrix a b c d) = show a ++ "\t" ++ show b ++ "\n" ++ show c ++ "\t" ++ show d

instance Num Matrix where
    fromInteger n = Matrix n n n n

    negate (Matrix a b c d) = Matrix (-a) (-b) (-c) (-d)

    (Matrix a1 b1 c1 d1) + (Matrix a2 b2 c2 d2) = Matrix (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

    (Matrix a1 b1 c1 d1) * (Matrix a2 b2 c2 d2) = Matrix (a1 * a2 + b1 * c2) (a1 * b2 + b1 * d2) 
                                                         (c1 * a2 + d1 * c2) (c1 * b2 + d1 * d2)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = get2nd $ Matrix 1 1 1 0 ^ n
    where
        get2nd :: Matrix -> Integer
        get2nd (Matrix a b c d) = b
