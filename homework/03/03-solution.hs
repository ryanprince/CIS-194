{-# OPTIONS_GHC -Wall #-}

-- Exercise 1: Hopscotch
s :: [a] -> Int -> [a]
s v n = map (v!!) [n-1,2*n-1..length v - 1]

skips :: [a] -> [[a]]
skips v = map (s v) [1..length v]

-- Exercise 2: Local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:ds)
  | a < b && b > c = b : localMaxima (b:c:ds)
  | otherwise      = localMaxima (b:c:ds)
localMaxima _ = []

-- Exercise 3: Histogram
histogram :: [Integer] -> String
histogram v = q $ map (\i -> fromIntegral (length (filter (==i) v))) [0..9]
q :: [Integer] -> String
q v = let m       = maximum v
          z y n x = if x == m then y else n
          l       = map (z '*' ' ') v
          w       = map (\x -> z (x - 1) x x) v
      in '\n' : if m == 0 then "==========\n0123456789\n" else l ++ q w

-- a = skips "ABCD"
-- b = skips "hello!"
-- c = skips [1]
-- d = skips [True, False]
-- e = skips []
--
-- f = localMaxima [2,9,5,6,1]
-- g = localMaxima [2,3,4,1,5]
-- h = localMaxima [1,2,3,4,5]
--
-- histA = putStr (histogram [1,1,1,5])
-- histB = putStr (histogram [1,4,5,4,6,6,3,4,2,4,9])
