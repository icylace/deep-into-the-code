module Arithmetic where

-- The functions `div` and `quot` both do integer division and behave
-- almost the same way.

_ = div 1 1
_ = quot 1 1
-- Both results will be `1`.

-- The difference being that `div` rounds toward negative infinity while
-- `quot` rounds toward zero.

_ = div 20 (-6)
-- Result will be `-4`.

_ = quot 20 (-6)
-- Result will be `-3`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The `rem` (remainder) function returns the remainder of a division.
-- The `mod` (modulo) function does the same but for modular division.

_ = rem 15 12
_ = mod 15 12
-- Both results will be `3`.

-- In Haskell, if one or both of the arguments are negative, `rem` will have
-- a result with the same sign as the dividend, while `mod` will have a
-- result with the same sign as the divisor.

_ = rem (-5) 2
-- Result will be `-1`.

_ = rem 5 (-2)
-- Result will be `1`.

_ = rem (-5) (-2)
-- Result will be `-1`.

_ = mod (-5) 2
-- Result will be `1`.

_ = mod 5 (-2)
-- Result will be `-1`.

_ = mod (-5) (-2)
-- Result will be `-1`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The negation operator is unary because it has just one operand.

_ = -100

-- Notice how it doesn't need a space between itself and its operand.

-- Attempting to use a negative number directly will cause an error.
--
--     9 + -100
--

-- We need parentheses to make it work.

_ = 9 + (-100)
-- Result will be `-91`.

-- The negation operator is also syntactic sugar for the `negate` function.
-- Syntactic sugar is "sweet" shorthand syntax designed to make typing and
-- reading code easier.

_ = 9 + (negate 100)
-- Result will be `-91`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

main :: IO ()
main = print ()

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Unary operator: An operator that applies to a single operand.
-- Syntax: The grammar and structure of the text used to express programs.
-- Syntactic sugar: Syntax designed to make code easier to read and write.
