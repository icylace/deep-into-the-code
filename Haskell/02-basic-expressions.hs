-- Here is a declaration that assigns the variable `a` to the expression `1`.
a = 1

a' = 1         -- Single-line comments can be placed after expressions.
a'' = 1        {- Block comments can also be placed after expressions. -}

a''' {- However, unlike single-line comments, -} = 0
a'''' = {- block comments can be placed in weird places! -} 0

-- A variable can be evaluated later on by referencing its name.

-- Evaluation is the process of simplifying an expression.  Expressions
-- that cannot be further simplified are in normal form and are usually
-- referred to as values.

-- Evaluating `a` will result in `1` which means `a` was already in normal form
-- and has the value `1`.

-- This declaration assigns the variable `b` to the expression `1 + 2`.
b = 1 + 2

-- Evaluating `b` will result in `3` which means `b` was a reducible expression
-- (a.k.a. redex).

-- An expression is not evaluated until it is forced to be by other expressions
-- that refer to it.  This is called lazy evaluation.

-- Key Terms
-- =========
-- Expression: A combination of terms able to be evaluated together.
-- Reducible expression (redex): An expression that can be further simplified.
-- Evaluation (reduction): The process of simplifying an expression.
-- Lazy evaluation: The delaying of evaluation until forced to do it.
-- Normal form: The most simplified, or irreducible, state of an expression.
-- Value: An expression in normal form.
-- Variable: A placeholder for an expression.
-- Declaration: A top-level assignment that binds a variable to an expression.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Arithmetic expressions follow the order of operations.
c = 1 + 2 * 3
-- Result will be `7`.

-- Parentheses may be used to nest expressions within other expressions.
-- Nested expressions are evaluated first.
c' = (1 + 2) * 3
-- Result will be `9`.

-- Nested expressions at the same level of nesting are evaluated from
-- left to right.
c'' = (1 + 2) * (3 + 4) * (5 + 6)
-- Result will be `231`.

-- Nesting is unlimited.
c''' = ((1 + 2) * 3) + 4 * (5 + 6)
-- Result will be `53`.

-- An expression will not necessarily be reduced to a value when evaluated.
-- By default it gets evaluated to weak head normal form, which is just a
-- more simplified version of the expression.

-- Key Terms
-- =========
-- Nested expression: An expression embedded within another expression.
-- Weak head normal form: A reduced state of an expression.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Here we declare a function called `double` that accepts a formal parameter
-- named `x` and returns the doubling of `x`.
double x = x * 2

-- Function names begin with a lowercase letter.

-- `double` is a pure function which means it will return the same output
-- when applied to the same input.
d = double 3
-- Result will be `6`.

-- An application of a pure function can be replaced by the function's
-- definition and still get the same result.
d' = 3 * 2
-- Result will be `6`.

-- A function must be applied to all of its arguments before being evaluated.

-- Key Terms
-- =========
-- Function: An expression returning a result when applied to an expression.
-- Argument: An input expression to an applied function.
-- Parameter (formal parameter): A variable for an argument to a function.
-- Pure function: A function returning the same result given the same argument.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Functions that are written between their arguments (e.g. arithmetic
-- operators) are said to be written in infix notation.  Functions
-- written this way by default are called operators and their
-- arguments are called operands.
e = 10 / 4
-- Result will be `2.5`.

-- Parentheses lets us use operators in prefix notation.
e' = (/) 10 4
-- Result will be `2.5`.

-- A standard Haskell library, Prelude, provides a `div` function which accepts
-- two arguments and does integer division with them.  Like other non-operator
-- functions it is written in prefix notation by default which means it
-- appears before its arguments.
f = div 10 4
-- Result will be `2`.

-- Backticks lets us use most prefix functions in infix notation.
f' = 10 `div` 4
-- Result will be `2`.

-- Though probably useless, a non-operator function can be wrapped in
-- parentheses and still be used in prefix notation.
f'' = (div) 10 4
-- Result will be `2`.

-- For those curious, the following constructions are not valid:
-- f''' = (`div`) 10 4
-- f'''' = 10 `(/)` 4

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
g = 2 + 3 + 4
g' = (2 + 3) + 4
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
h = 2 ^ 3 ^ 4
h' = 2 ^ (3 ^ 4)
-- Both results will be `2417851639229258349412352`.

h'' = (2 ^ 3) ^ 4
-- Result will be `4096`.

-- Key Terms
-- =========
-- Associativity: The way operators are grouped in the absence of parentheses.
-- Precedence: The order that similarly-grouped operators are parsed.
