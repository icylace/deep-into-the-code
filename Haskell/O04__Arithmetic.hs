module O04__Arithmetic () where

-- `div` and `quot` both do integer division and behave almost the same way.

_ = div 1 1     -- `1`
_ = quot 1 1    -- `1`

-- `div` rounds toward negative infinity while `quot` rounds toward zero.

_ = div 20 (-6)     -- `-4`
_ = quot 20 (-6)    -- `-3`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- `rem` (remainder) returns the remainder of a division.
-- `mod` (modulo) does the same but for modular division.

_ = rem 15 12   -- `3`
_ = mod 15 12   -- `3`

-- In Haskell, if one or both of the arguments are negative, `rem` will have a
-- result with the same sign as the dividend, while `mod` will have a result
-- with the same sign as the divisor.

_ = rem (-5) 2      -- `-1`
_ = rem 5 (-2)      -- `1`
_ = rem (-5) (-2)   -- `-1`

_ = mod (-5) 2      -- `1`
_ = mod 5 (-2)      -- `-1`
_ = mod (-5) (-2)   -- `-1`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The negation operator, `-`,  is unary because it has just one operand.

_ = -100    -- `-100`
_ = - 100   -- `-100`


{-
_ = 9 + -100    -- Causes a compile-time error.
-}

-- We need parentheses to make it work.

_ = 9 + (-100)    -- `-91`

-- The negation operator is also syntactic sugar for the `negate` function.

_ = 9 + negate 100    -- `-91`

-- Syntactic sugar is "sweet" shorthand syntax designed to make typing and
-- reading code easier.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Unary operator: An operator that applies to a single operand.
-- Syntax: The grammar and structure of the text used to express programs.
-- Syntactic sugar: Syntax designed to make code easier to read and write.
