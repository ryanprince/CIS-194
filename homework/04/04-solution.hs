{-# OPTIONS_GHC -Wall #-}

import Data.List

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
insertBalanced n@(Node 0 Leaf _ Leaf) Leaf = n
insertBalanced n@(Node 0 Leaf _ Leaf) (Node h l v r)
  | minHeight l < (h-1) = Node h (insertBalanced n l) v r
  | minHeight r < (h-1) = Node h l v (insertBalanced n r)
  | otherwise           = Node (h+1) (insertBalanced n l) v r
insertBalanced _ t = t

foldTree :: [a] -> Tree a
foldTree = foldr insertBalanced Leaf . map (\v -> Node 0 Leaf v Leaf)

-- Exercise 3: More folds!
-- Returns True if there are an odd number of True values, and False otherwise.
xor :: [Bool] -> Bool
xor = odd . foldr (\b n -> if b then (n+1) else n) 0

-- The standard map function implemented using a fold.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- Exercise 4: Finding primes
-- Given an integer, generate all the odd primes up to 2n + 2.
sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let numbers   = [1..n]
      crossOff  = nub [m | j <- [1..n], i <- [1..j], let m = i+j+2*i*j, m <= n]
      numbers'  = filter (\e -> not (e `elem` crossOff)) numbers
  in map (\m -> 2 * m + 1) numbers'

-- The function cartProd was given, though I didn't want to use it.
-- cartProd :: [a] -> [b] -> [(a, b)]
-- cartProd xs ys = [(x, y) | x <- xs, y <- ys]

-- I wrote this for the fliter step, thinking it would be faster than testing
-- (e `elem` crossOff) for n times, but in my timing tests, the filter version
-- works just as well.
-- removeItems :: Ord a => [a] -> [a] -> [a]
-- removeItems fromList subtractList =
--   let removeItemsSorted xs [] = xs
--       removeItemsSorted [] _  = []
--       removeItemsSorted (x:xs) (y:ys)
--         | x < y     = x : removeItemsSorted xs (y:ys)
--         | x > y     = removeItemsSorted (x:xs) ys
--         | otherwise = removeItemsSorted xs (y:ys)
--   in removeItemsSorted (sort fromList) (sort subtractList)
