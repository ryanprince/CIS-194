{-# OPTIONS_GHC -Wall #-}

-- Exercise 1: Wholemeal programming
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (+(-2))  . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

hailstone :: Integer -> Integer
hailstone n
  | even n    = n `div` 2
  | otherwise = 3 * n + 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate hailstone

-- Exercise 2: Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- It's interesting that the Leaf node has a minHeight of -1 to make this work.
minHeight :: Tree a -> Integer
minHeight Leaf                                         = -1
minHeight (Node _ l@(Node _ _ _ _) _ r@(Node _ _ _ _)) = 1 + min (minHeight l) (minHeight r)
minHeight _                                            = 0

insertBalanced :: Tree a -> Tree a -> Tree a
insertBalanced n@(Node 0 _ _ _) Leaf = n
insertBalanced n@(Node 0 _ _ _) (Node h l v r)
  | minHeight l < (h-1) = Node h (insertBalanced n l) v r
  | minHeight r < (h-1) = Node h l v (insertBalanced n r)
  | otherwise           = Node (h+1) (insertBalanced n r) v l
insertBalanced _ t = t

foldTree :: [a] -> Tree a
foldTree = foldr insertBalanced Leaf . map (\v -> Node 0 Leaf v Leaf)
