nums :: [Int]
nums = [1..999]

mult :: Int -> Bool
mult n = n `mod` 3 == 0 || n `mod` 5 == 0

mults = filter mult nums

suma = foldr (+) 0 mults

main = print $ suma
