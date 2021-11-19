import Debug.Trace

fac :: Int -> Int
fac 0 = 1
fac n = n * (fac n)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = (fib n) + (fib (n-1))

reversa :: [Int] -> [Int] -> [Int]
reversa [] acc = acc
reversa (x:xs) acc = let rest = (x:acc) in
  reversa xs (trace ("valor acc: " ++ (show rest)) rest)

data Tree a
  = Empty
  | Node {root:: a, left::Tree a, right::Tree a} deriving (Show, Eq)

level :: Eq a => Tree a -> a -> Maybe Int
level Empty _ = Nothing
level t e
  | root t == e = Just 0
  | otherwise = procesaPar (level (left t) e) (level (right t) e)

procesaPar :: Maybe Int -> Maybe Int -> Maybe Int
procesaPar Nothing Nothing = Nothing
