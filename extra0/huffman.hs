freqs :: Eq a => [a] -> [(Int, a)]
freqs [] = []
freqs (x:xs) = (length xLike + 1, x) : freqs notX
  where xLike = filter (==x) xs; notX = filter (/=x) xs

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort less ++ [x] ++ sort more
  where less = filter (<=x) xs; more = filter (>x) xs

data Tree a
  = Leaf {freq :: Int, val :: a}
  | Node {freq :: Int, left :: Tree a, right :: Tree a} deriving (Show)

buildLeaf :: (Int, a) -> Tree a
buildLeaf (k, x) = Leaf k x

merge :: Tree a -> Tree a -> Tree a
merge t1 t2 = Node (freq t1 + freq t2) t1 t2

addOrd :: Ord a => a -> [a] -> [a]
addOrd k [] = [k]
addOrd k (x:xs) =
  if k <= x
  then k:x:xs
  else x : addOrd k xs

buildTree :: Ord a => [Tree a] -> Tree a
buildTree [] = error "no values to build tree"
buildTree [x] = x
buildTree (x:y:xs) = buildTree $ addOrd (merge x y) xs

fromList :: Ord a => [a] -> Tree a
fromList = buildTree . sort . map buildLeaf . freqs

instance Eq (Tree a) where
  t1 == t2 = freq t1 == freq t2

instance Ord (Tree a) where
  t1 <= t2 = freq t1 <= freq t2

data Bit = Zero | One

instance Show Bit where
  show Zero = "0"
  show One = "1"

encode :: Ord a => [a] -> (Tree a, [Bit])
encode xs = (tree, bits)
  where
    tree = fromList xs
    bits = concatMap (findFirst . getCodes $ tree) xs

findFirst :: Eq a => [(a, b)] -> a -> b
findFirst [] a = error "no in list"
findFirst ((u, v):ys) x = if x == u then v else findFirst ys x

getCodes :: Ord a => Tree a -> [(a, [Bit])]
getCodes (Leaf _ v) = [(v, [])]
getCodes (Node _ l r) = add Zero (getCodes l) ++ add One (getCodes r)

add :: a -> [(b, [a])] -> [(b, [a])]
add x = map (\(y, xs) -> (y, (x:xs)))

decode :: Ord a => [Bit] -> Tree a -> [a]
decode bits t = matchAll bits t
  where
    matchAll bits (Leaf _ val) = val : matchAll bits t
    matchAll (Zero:bits) (Node _ l _) = matchAll bits l
    matchAll (One:bits) (Node _ _ r) = matchAll bits r
    matchAll [] _ = []

main :: IO ()
main = putStrLn "Hola"
