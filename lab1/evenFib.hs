fib :: Int -> Int
fib 1 = 1
fib 2 = 2
fib n = fib (n-1) + fib (n-2)

fibs10 = map fib [1..10]

next :: [Int] -> [Int]
next (a:b:xs) = a+b:a:b:xs
