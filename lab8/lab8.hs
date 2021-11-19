data Tree a
  = Empty
  | Node { root:: a
         , left::Tree a
         , right::Tree a}
  deriving (Eq)

instance Show a => Show (Tree a) where
  show t = unlines . showLevels $ t

showLevels :: Show a => Tree a -> [String]
showLevels Empty = ["empty"]
showLevels (Node e l r) = show e : tabSubTrees l r

-- tab subtrees using file-tree like symbols and indendation
tabSubTrees :: Show a => Tree a -> Tree a -> [String]
tabSubTrees l r =
  (showPr "├── " "│   " l) ++ (showPr "└── " "    " r)
  where showPr f r = tabPriority f r . showLevels

tabPriority :: String -> String -> [String] -> [String]
tabPriority _ _ [] = []
tabPriority f r (x:xs) = (f ++ x) : (map (r ++) xs)

level :: Eq a => Tree a -> a -> Maybe Int
level Empty _ = Nothing
level (Node e l r) x =
  processSubtrees (level l e) (level r e)

processSubtrees :: Maybe Int -> Maybe Int -> Maybe Int
processSubtrees Nothing Nothing = Nothing
processSubtrees Nothing (Just n) = Just $ n + 1
processSubtrees (Just n) Nothing = Just $ n + 1
processSubtrees (Just n) (Just m) = Just $ (max n m) + 1

{--
level' :: Eq a => Tree a -> a -> Maybe Int
level' Empty _ = Nothing
level' (Node e l r) x =
  case (level' l, level' r) of
    (Nothing, Nothing) -> Nothing
    (Just n, Nothing) -> Just $ n + 1
-}

size :: Tree a -> Int
size Empty =  0
size (Node _  l r) = 1 + size l + size r

full :: Tree a -> Maybe Int
full Empty = Just 0
full (Node _ Empty (Node _ _ _)) = Nothing
full (Node _ (Node _ _ _) Empty) = Nothing
full (Node _ l r) = (+1) <$> ((+) <$> full l <*> full r)

balance :: Tree a -> Maybe (Tree Int)
balance Empty = Just Empty
balance (Node _ l r) =
  let diff = size l - size r in
    if abs diff <= 1
    then Node diff <$> (balance r) <*> (balance l)
    else Nothing
