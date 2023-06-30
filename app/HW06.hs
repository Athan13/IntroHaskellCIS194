-- EXERCISE 1
fib :: Integer -> Integer
fib n
    | n <= 1 = n
    | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- EXERCISE 2
fibs2 :: [Integer]
fibs2 = (\ns -> ns ++ [last ns + last (init ns)]) [0, 1]
