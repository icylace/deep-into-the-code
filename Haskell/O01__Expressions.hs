module O01__Expressions where

-- Here is a declaration for a variable identified as `a` with expression `1`.

a = 1

a1 = 1    -- Single-line comments can be placed after expressions.
a2 = 1    {- Block comments can also be placed after expressions. -}

a3 {- However, unlike single-line comments, -} = 0
a4 = {- block comments can be placed in weird places! -} 0

-- Variable names can contain letters, numbers, underscores, and apostrophes
-- but they must always start with a lowercase letter or an underscore.

a5' = 1
a'5' = 1
a_5 = 1
a_5' = 1
a_'5' = 1
a5'' = 1
aB'C = 1

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

c = 3 + 4   -- `7`
d = c + 5   -- `12`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- An underscore may be used to indicate that we do not wish to save an
-- expression for further evaluation.

_ = 2 + 3

-- This is normally useless but my notes about Haskell uses this trick a
-- lot as a way for me to avoiding having to constantly come up with new
-- variable names.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Arithmetic expressions follow the order of operations.

_ = 1 + 2 * 3   -- `7`

-- Parentheses may be used to nest expressions within other expressions.
-- Nested expressions are evaluated first.

_ = (1 + 2) * 3   -- `9`

-- Nested expressions at the same level of nesting are evaluated from
-- left to right.

_ = (1 + 2) * (3 + 4) * (5 + 6)   -- `231`

-- Nesting is unlimited.

_ = ((1 + 2) * 3) + 4 * (5 + 6)   -- `53`
_ = ((1 + 2) * (3 * (8 + (2 * (5 + 4))))) + 4 * (5 + 6)   -- `278`

-- An expression will not necessarily be reduced to a value when evaluated.
-- By default it gets evaluated to weak head normal form, which is just a
-- more simplified version of the expression.

-- An expression is not evaluated until it is forced to be by other
-- expressions that refer to it.  This is called lazy evaluation.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Expression: A combination of terms able to be evaluated together.
-- Nested expression: An expression embedded within another expression.
-- Reducible expression (redex): An expression that can be further simplified.
-- Weak head normal form: A reduced state of an expression.
-- Normal form: The most simplified, or irreducible, form of an expression.
-- Value: An expression in normal form.

-- Evaluation (reduction): The process of simplifying an expression.
-- Lazy evaluation: The delaying of evaluation until forced to do it.

-- Identifier: A name that references an expression.
-- Bind: To assign an identifier to an expression.
-- Binding: An identified expression.
-- Variable: A simple binding.
-- Declaration: A binding.
