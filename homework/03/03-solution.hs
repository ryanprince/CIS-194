{-# OPTIONS_GHC -Wall #-}

-- Exercise 1: Hopscotch
skips :: [a] -> [[a]]
skips v = map (\j -> map snd (filter (\(i, _) -> i `mod` j == 0) (zip [1..] v))) [1..length v]

-- Exercise 2: Local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:ds)
  | a < b && b > c = b : localMaxima (b:c:ds)
  | otherwise      = localMaxima (b:c:ds)
localMaxima _ = []

-- Exercise 3: Histogram
histogram :: [Integer] -> String
histogram v = _h $ map (\i -> fromIntegral (length (filter (==i) v))) [0..9]
_h :: [Integer] -> String
_h v = let m = maximum v
           l = map (\x -> if x == m then '*' else ' ') v
           w = map (\x -> if x == m then x - 1 else x) v
       in '\n' : if m == 0 then "==========\n0123456789\n" else l ++ _h w

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
