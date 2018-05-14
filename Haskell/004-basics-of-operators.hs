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

-- A partially applied operator is called a section and the act of creating a
-- section is called sectioning.

_ = 2 * 5
_ = (2 *) 5
_ = timesTwo 5
_ = (* 5) 2
_ = timesFive 2
-- All five results will be `10`.

-- Notice that `2 (* 5)` was not included in the previous list.  This is because
-- `2` would have been in the spot where a function or a function name should be
-- and an attempt would have been made to apply it to the argument `(* 5)`.
-- Since `2` is not a function this would cause an error.

-- The negation and subtraction operators use the same symbol, `-`.  This causes
-- an issue when trying to sectioning the second operand of subtraction.  The
-- expression `(- 2) 1` will cause an error because negation will be used
-- when `-` is applied to a single argument.  The fix is to use another
-- function from the standard Prelude library named `subtract`.
_ = (subtract 2) 1
-- Result will be `-1`.

-- Key Terms
-- =========
-- Section: A partial application of an infix function.
-- Sectioning: Partially applying an infix function.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The application operator, `$`, allows everything to its right to be evaluated
-- first and can be used to delay funciton application.
_ = 2 * (3 + 5)
_ = (2 *) (3 + 5)
_ = (2 *) $ (3 + 5)
_ = (2 *) $ 3 + 5
-- All four results will be `16`.

-- Contrast the previous with the following which uses neither parentheses nor
-- the application operator.
_ = (2 *) 3 + 5
-- Result will be `11`.

-- The application operator can be used to reduce the number of parentheses
-- required for a given expression.
_ = negate (negate (negate (negate (negate (negate (negate (negate 1)))))))
_ = negate $ negate $ negate $ negate $ negate $ negate $ negate $ negate 1
-- Both results will be `1`.

-- Running `:i $` in GHCi will show that the application operator is a right
-- associative infix operator with a precedence level of 0.
