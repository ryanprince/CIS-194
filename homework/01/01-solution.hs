{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
-- Separates the digits of a number into a list of integers.
-- For example, toDigits 1234 == [1,2,3,4].
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = let (quotient, remainder) = n `divMod` 10
                in remainder : toDigitsRev quotient

-- Exercise 2
-- Doubles every second number in the list, moving from right to left.
-- For example, [1,1,1] == [1,2,1], [1,1,1,1] = [2,1,2,1].
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherRev (reverse xs))
  where doubleEveryOtherRev (a:b:xs') = a : (2*b) : doubleEveryOtherRev xs'
        doubleEveryOtherRev xs'       = xs'

-- Exercise 3
-- Sums all of the digits that are in the list of inegers.
-- For example, [1,22,4] == 1 + 2 + 2 + 4 = 9.
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map (sum . toDigits) xs)

-- Exercise 4
-- Validates a credit card number, using the standard technique.
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0

-- Exercise 5
-- Produces the list of moves that will solve the tower of haoni puzzle, moving
-- all n discs from the first peg to the second. Move sequence for solving the
-- haoni puzzle is defined recursively as follows.
--   1. Move n - 1 discs from a to c using b as temporary storage.
--   2. Move the bottom disc from a to b.
--   3. Move n - 1 discs from c to b using a as temporary storage.
-- Example: hanoi 2 "a" "b" "c" == [("a","c"),("a","b"),("c","b")]
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0    = []
  | otherwise = hanoi (n - 1) a c b ++ (a, b) : hanoi (n - 1) c b a
