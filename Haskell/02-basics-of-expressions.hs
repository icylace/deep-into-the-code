-- Here is a declaration that assigns the variable `a` to the expression `1`.
a = 1

a1 = 1         -- Single-line comments can be placed after expressions.
a2 = 1        {- Block comments can also be placed after expressions. -}

a3 {- However, unlike single-line comments, -} = 0
a4 = {- block comments can be placed in weird places! -} 0

-- Variable names can contain letters, numbers, underscores, and apostrophes but they must
-- always start with a lowercase letter.
a5' = 1
a'5' = 1
a_5 = 1
a_5' = 1
a_'5' = 1
a5'' = 1

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

-- Key Terms
-- =========
-- Expression: A combination of terms able to be evaluated together.
-- Reducible expression (redex): An expression that can be further simplified.
-- Evaluation (reduction): The process of simplifying an expression.
-- Normal form: The most simplified, or irreducible, state of an expression.
-- Value: An expression in normal form.
-- Variable: A placeholder for an expression.
-- Declaration: A top-level assignment that binds a variable to an expression.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- An underscore may be used to indicate that we do not wish to save the result
-- of evaluating an expression.
_ = 2 + 3

-- It's rare that this would be a good idea.  That said, these notes about
-- Haskell uses this trick a lot.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Arithmetic expressions follow the order of operations.
_ = 1 + 2 * 3
-- Result will be `7`.

-- Parentheses may be used to nest expressions within other expressions.
-- Nested expressions are evaluated first.
_ = (1 + 2) * 3
-- Result will be `9`.

-- Nested expressions at the same level of nesting are evaluated from
-- left to right.
_ = (1 + 2) * (3 + 4) * (5 + 6)
-- Result will be `231`.

-- Nesting is unlimited.
_ = ((1 + 2) * 3) + 4 * (5 + 6)
-- Result will be `53`.

-- An expression will not necessarily be reduced to a value when evaluated.
-- By default it gets evaluated to weak head normal form, which is just a
-- more simplified version of the expression.

-- An expression is not evaluated until it is forced to be by other expressions
-- that refer to it.  This is called lazy evaluation.

-- Key Terms
-- =========
-- Nested expression: An expression embedded within another expression.
-- Weak head normal form: A reduced state of an expression.
-- Lazy evaluation: The delaying of evaluation until forced to do it.
