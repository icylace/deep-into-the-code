module O19__Recursion () where

-- "Recursion" is defining a function in terms of itself via self-referential
-- expressions.  Such functions are said to be "recursive".

-- Recursion is useful for expressing indefinite or incremental computation.
-- This avoids unnecessary repetition of code and allows the data being
-- processed to determine when the computation is done.

fourFactorial :: Integer
fourFactorial = 4 * 3 * 2 * 1

-- This won't work. It never stops.
brokenFact1 :: Integer -> Integer
brokenFact1 n = n * brokenFact1 (n - 1)

-- Actually attempting to use `brokenFact1` will cause an infinite loop.

{-
_ = brokenFact1 4

-- brokenFact1 4 =
-- 4 * (4 - 1)
-- * ((4 - 1) - 1)
-- * (((4 - 1) - 1) - 1)
-- * ((((4 - 1) - 1) - 1) - 1)
-- * (((((4 - 1) - 1) - 1) - 1) - 1)
-- ... never stops
-}

-- A "base case" is an input case to a recursive function that does not apply
-- that function to further arguments. It's used to prevent infinite loops.

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

_ = factorial 4    -- `24`

-- factorial 4 =
--   4 * factorial (4 - 1)
--   4 * factorial 3
--   4 * 3 * factorial (3 - 1)
--   4 * 3 * factorial 2
--   4 * 3 * 2 * factorial (2 - 1)
--   4 * 3 * 2 * factorial 1
--   4 * 3 * 2 * 1 * factorial (1 - 1)
--   4 * 3 * 2 * 1 * factorial 0
--   4 * 3 * 2 * 1 * 1
--   24





fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

_ = fibonacci 6   -- `8`

-- fibonacci 6 = fibonacci 5 + fibonacci 4
-- fibonacci 5 = fibonacci 4 + fibonacci 3
-- fibonacci 4 = fibonacci 3 + fibonacci 2
-- fibonacci 3 = fibonacci 2 + fibonacci 1
-- fibonacci 2 = fibonacci 1 + fibonacci 0

-- fibonacci 0 +           0
-- fibonacci 1 +           1
-- fibonacci 2 + (1 + 0 =) 1
-- fibonacci 3 + (1 + 1 =) 2
-- fibonacci 4 + (1 + 2 =) 3
-- fibonacci 5 = (2 + 3 =) 5
-- fibonacci 6 = (3 + 5 =) 8









-- not recursive
lessOne :: Int -> Int
lessOne n = n - 1

-- recursive
zero :: Int -> Int
zero 0 = 0
zero n = zero (n - 1)













-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

Recursion: Defining a function with self-referential expressions.
Recursion: Self-referential composition.

Recursive function: A function that composes with itself.
Base case: A stopping point for a recursive function.

-}
