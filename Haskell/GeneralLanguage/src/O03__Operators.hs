module O03__Operators () where

-- Functions appearing between their arguments (e.g. arithmetic operators) are
-- said to be written in infix notation.

-- Operators are functions with non-alphanumeric names and are written in infix
-- notation by default. Their arguments are called operands. Most operators
-- are binary meaning they apply to a couple arguments.

_ = 10 / 4    -- `2.5`

-- Parentheses lets us use operators in prefix notation.

_ = (/) 10 4    -- `2.5`

-- A standard Haskell library, Prelude, provides a `div` function which accepts
-- two arguments and does integer division with them. Like other non-operator
-- functions it is written in prefix notation by default which means it
-- appears before its arguments.

_ = div 10 4    -- `2`

-- Backticks lets us use prefix functions in infix notation.

_ = 10 `div` 4    -- `2`

-- We can influence the readability of an expression by switching notations.

_ = (/) ((/) 10 4) 25    -- `0.1`
_ = 10 / 4 / 25          -- `0.1`

_ = div (div 10 4) 25      -- `0`
_ = 10 `div` 4 `div` 25    -- `0`

-- Though probably useless, a non-operator function can be wrapped in
-- parentheses and still be used in prefix notation.

_ = (div) 10 4      -- `2`
_ = ((div) 10) 4    -- `2`

-- Backticks also allow "sectioning" to be used. More on that later.

_ = (10 `div`) 4    -- `2`
_ = (`div` 4) 10    -- `2`

{-
_ = (`div`) 10 4    -- Error.
_ = 10 (`div`) 4    -- Error.
_ = `(/)` 10 4      -- Error.
_ = 10 `(/)` 4      -- Error.
-}

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Infix operators have the properties of associativity and precedence.

-- Haskell's REPL (read-eval-print loop) environment, GHCi, has an `:i` (short
-- for `:info`) command that can give some information about types
-- and expressions.

{- GHCi ------------------------------------------------------------------------

> :i +
class Num a where
  (+) :: a -> a -> a
  ...
        -- Defined in ‘GHC.Num’
infixl 6 +

-------------------------------------------------------------------------------}

-- `infixl` means left associative infix operator. The `6` is the precedence
-- level. Higher ones are applied first, on a scale from 0 to 9. The `+` is
-- just the name of the operator.

-- The subtraction operator is very similar with `infixl 6 -`.

-- The multipication operator has similar associativity but higher precedence
-- with `infixl 7 *`.

-- These two variables are evaluated equivalently due to left associativity.

_ = 2 - 3 + 4      -- `3`
_ = (2 - 3) + 4    -- `3`

-- Parentheses can be used to simulate right associativity.

_ = 2 - (3 + 4)   -- `-5`

-- The exponentation operator has right associativity and an even higher
-- precedence with `infixr 8 ^`.

_ = 2 ^ 3 ^ 4      -- `2417851639229258349412352`
_ = 2 ^ (3 ^ 4)    -- `2417851639229258349412352`
_ = (2 ^ 3) ^ 4    -- `4096`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Like any other function, operators can be partially applied.

timesTwo = (2 *)
-- Result will be a function similar to the multiplication operator but lacking
-- the first operand parameter and having the value `2` exist where that first
-- operand originally was.

-- The other operand can also be partially applied.

timesFive = (* 5)
-- Result will be a function similar to the multiplication operator but lacking
-- the second operand parameter and having the value `5` exist where that second
-- operand originally was.

-- A partially applied operator is called a "section" and the act of creating a
-- section is called "sectioning".

_ = 2 * 5          -- `10`
_ = (2 *) 5        -- `10`
_ = timesTwo 5     -- `10`
_ = (* 5) 2        -- `10`
_ = timesFive 2    -- `10`

{-
-- Here `2` is in the spot where a function or function name should be.
_ = 2 (* 5)        -- Error.
-}

{-
-- The negation and subtraction operators use the same symbol, `-`. This causes
-- an issue when trying to section the second operand of subtraction. Negation
-- will be used when `-` is applied to a single argument. The fix is to use
-- another function from the standard Prelude library named `subtract`.

_ = (- 2) 1        -- Error.

-}

_ = (subtract 2) 1    -- `-1`
_ = subtract 2 1      -- `-1`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The application operator, `$`, allows everything to its right to be evaluated
-- first and can be used to delay function application.

_ = 2 * (3 + 5)        -- `16`
_ = (2 *) (3 + 5)      -- `16`
_ = (2 *) $ (3 + 5)    -- `16`
_ = (2 *) $ 3 + 5      -- `16`

_ = (2 *) 3 + 5    -- `11`

_ = negate (negate (negate (negate (negate (negate (negate (negate 1)))))))      -- `1`
_ = (negate . negate . negate . negate . negate . negate . negate . negate) 1    -- `1`
_ = negate . negate . negate . negate . negate . negate . negate . negate $ 1    -- `1`
_ = negate $ negate $ negate $ negate $ negate $ negate $ negate $ negate 1      -- `1`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

Prefix notation: The placement of a function before its arguments.
Infix notation: The placement of a function between its arguments.
Operator: A function with a non-alphanumeric name and infix by default.
Operand: An argument to an operator.
Binary operator: An operator that applies to a couple operands.
Prelude: Haskell's standard library of functions.

Associativity: The way operators are grouped in the absence of parentheses.
Precedence: The order that similarly-grouped operators are parsed.

Section: A partial application of an infix function.
Sectioning: Partially applying an infix function.

-}
