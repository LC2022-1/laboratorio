sumN :: Int -> Int
sumN n = n * (n + 1) `div` 2

sumNSqr :: Int -> Int
sumNSqr n = n * (n + 1) * (2 * n + 1) `div` 6

squareDiff :: Int -> Int
squareDiff n = (sumN n)^2 - (sumNSqr n)

main = print $ squareDiff 100
