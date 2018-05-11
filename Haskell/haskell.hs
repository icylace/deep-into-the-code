-- This is a single-line comment.

-- Comments let you put helpful commentary with your code.

{- Block comments are
usually seen spanning
multiple lines. -}

{- Block comments can be used like single-line comments. -}

{- Block comments can be -}  {- placed next to each other. -}

{- Not that you
would -}{- want
to do that. -}

{- Also, block comments...
  {- ...may be nested. -}
-}

      -- Single-line comments don't need to start at the beginning of the line.

      {- Neither do block comments. -}

-- Key Terms
-- =========
-- Comment: Text meant to be read by humans and ignored by the compiler.
-- Single-line comment: A comment that cannot be on more than one line.
-- Block comment: A comment that can extend to multiple consecutive lines.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Here is a declaration that assigns the name `a` to the expression `1`.
a = 1

a' = 1         -- Single-line comments can be placed after expressions.
a'' = 1        {- Block comments can also be placed after expressions. -}

{- However, unlike single-line comments, -} a''' = 0
a'''' {- block comments can be placed in weird -} = 0
a''''' = {- places but please don't do that! -} 0

-- A named expression can be evaluated later on by referencing its name.
-- Evaluation is the process of simplifying an expression.  Expressions
-- that cannot be further simplified are in normal form and are usually
-- referred to as values.

-- Evaluating `a` will result in `1` which means `a` was already in normal form
-- and has the value `1`.

-- This declaration assigns the name `b` to the expression `1 + 2`.
b = 1 + 2

-- Evaluating `b` will result in `3` which means `b` was a reducible expression
-- (a.k.a. redex).

-- An expression is not evaluated until it is forced to be by other expressions
-- that refer to it.  This is called lazy evaluation.

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
-- Expression: A combination of terms able to be evaluated together.
-- Nested expression: An expression embedded within another expression.
-- Reducible expression (redex): An expression that can be further simplified.
-- Evaluation (reduction): The process of simplifying an expression.
-- Lazy evaluation: The delaying of evaluation until forced to do it.
-- Weak head normal form: A simplified state of an expression.
-- Normal form: The most simplified, or irreducible, state of an expression.
-- Value: An expression in normal form.
-- Declaration: A statement that assigns a name to an expression.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Here we declare a function named `double` that accepts a parameter
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

-- A function must first be applied to its arguments before it can be evaluated.

-- Functions such as operators (e.g. arithmetic operators) that are written
-- in infix notation appear between their arguments.
e' = 3 * 2


-- Operators
f'' = 10 / 4
-- Result will be `2.5`.


-- There is a `div` function which accepts two arguments and does integer
-- division with them.  Like many other functions it is written in prefix
-- notation by default which means it appears before its arguments.
f = div 10 4
-- Result will be `2`.

-- Backticks let us use normal functions in infix notation.
f' = 10 `div` 4





-- Key Terms
-- =========
-- Function: An expression returning a result when applied to an expression.
-- Argument: An input expression to an applied function.
-- Variable: A placeholder for an expression.
-- Parameter: A variable for an argument to a function.
-- Pure function: A function returning the same result given the same argument.
-- Currying: Applying a series of nested functions to a series of arguments.
-- Partial application: The fixing of arguments to a function to produce
--     another function that takes the remaining unfixed paramters.
-- Prefix notation: The placement of a function before its arguments.
-- Infix notation: The placement of a function between its arguments.
-- Operator: A function with a non-alphanumeric name and infix by default.
-- Operand: An argument to an operator.
-- Syntactic sugar: Syntax designed to make expressions easier to work with.
-- Prelude: A library of standard functions.








--  Type - The kind of data that can be processed.





-- A Haskell source code file has the `.hs` extension.
test.hs

-- `sayHello` has the type of `String -> IO ()`
sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")





main :: IO ()
main = putStrLn "Hello World"
