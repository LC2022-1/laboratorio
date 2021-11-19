import Debug.Trace

fac :: Int -> Int
fac 0 = 1
fac n = n * (fac (n-1))

facTail :: Int -> Int -> Int
facTail 0 acc = acc
facTail n acc =
  facTail (n-1) (trace ("valor de acc: " ++ show (n*acc) ) (n*acc))

fac' :: Int -> Int
fac' n = facTail n 1

data Tree a
  = Empty
  | Node {root:: a, left::Tree a, right::Tree a} deriving (Show, Eq)

level :: Eq a => Tree a -> a -> Maybe Int
level Empty _ = Nothing
level (Node k l r) e =
  if k == e
  then Just 0
  else procesaRes (level l e) (level r e)

procesaRes :: Maybe Int -> Maybe Int -> Maybe Int
procesaRes Nothing Nothing = Nothing
procesaRes (Just n) Nothing = Just (n+1)
procesaRes Nothing (Just n) = Just (n+1)
procesaRes (Just n) (Just m) = Just $ (min n m) + 1
