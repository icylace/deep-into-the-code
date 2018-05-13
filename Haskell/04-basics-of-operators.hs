-- Functions that are written between their arguments (e.g. arithmetic
-- operators) are said to be written in infix notation.  Functions
-- written this way by default are called operators and their
-- arguments are called operands.
_ = 10 / 4
-- Result will be `2.5`.

-- Parentheses lets us use operators in prefix notation.
_ = (/) 10 4
-- Result will be `2.5`.

-- A standard Haskell library, Prelude, provides a `div` function which accepts
-- two arguments and does integer division with them.  Like other non-operator
-- functions it is written in prefix notation by default which means it
-- appears before its arguments.
_ = div 10 4
-- Result will be `2`.

-- Backticks lets us use most prefix functions in infix notation.
_ = 10 `div` 4
-- Result will be `2`.

-- Though probably useless, a non-operator function can be wrapped in
-- parentheses and still be used in prefix notation.
_ = (div) 10 4
-- Result will be `2`.

-- For those curious, the following constructions are invalid:
--
--     (`div`) 10 4
--
--     10 `(/)` 4
--

-- Key Terms
-- =========
-- Prefix notation: The placement of a function before its arguments.
-- Infix notation: The placement of a function between its arguments.
-- Operator: A function with a non-alphanumeric name and infix by default.
-- Operand: An argument to an operator.
-- Prelude: A library of standard functions.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Infix operators have the properties of associativity and precedence.

-- Haskell's REPL (read-eval-print loop) environment, GHCi, has a `:i` (`:info`)
-- command that gives some information about functions.

-- Running this in GHCi will show information about the addition operator:
--
--     :i +
--
-- The result will include:
--
--     infixl 6 +
--
-- `infixl` means left associative infix operator.  The `6` is the precedence
-- level.  Higher ones are applied first, on a scale from 0 to 9.  The `+` is
-- just the name of the operator.

-- These two variables are evaluated equivalently due to left associativity.
_ = 2 + 3 + 4
_ = (2 + 3) + 4
-- Both results will be `9`.

-- Running this in GHCi will show information about the exponentation operator:
--
--     :i ^
--
-- The result will include:
--
--     infixr 8 ^
--
-- Which means our `(^)` function is a right associative infix operator with a
-- precedence of 8.

-- These two variables are evaluated equivalently due to right associativity.
_ = 2 ^ 3 ^ 4
_ = 2 ^ (3 ^ 4)
-- Both results will be `2417851639229258349412352`.

_ = (2 ^ 3) ^ 4
-- Result will be `4096`.

-- Key Terms
-- =========
-- Associativity: The way operators are grouped in the absence of parentheses.
-- Precedence: The order that similarly-grouped operators are parsed.
