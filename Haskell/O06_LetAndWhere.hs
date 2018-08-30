module O06_LetAndWhere where

-- A let expression is an expression that has declarations introduced into it.
-- Declarations created in this way are able to be referenced only within the
-- scope of the expression they're associated to.

-- Syntactically, a let expression starts with the `let` keyword, followed by
-- declarations that are being introduced, followed by the `in` keyword, then
-- finally followed by the rest of the expression.

upOne n = let one = 1 in n + one
_ = upOne 23
-- Result will be `24`.

-- The declarations introduced by `let` can be parameterized.

upSome n m = let some = m in n + some
_ = upSome 3 9
-- Result will be `12`.

-- Multiple declarations on the same line must be separated with semicolons.

upOneAndSome n m = let one = 1; some = m in n + one + some
_ = upOneAndSome 3 9
-- Result will be `13`.

-- The `where` keyword has a similar purpose and scoping effect as `let` but
-- it instead defines declarations after its associated expression.

upOne' n = n + one where one = 1
_ = upOne' 23
-- Result will be `24`.

upSome' n m = n + some where some = m
_ = upSome' 3 9
-- Result will be `12`.

upOneAndSome' n m = n + one + some where one = 1; some = m
_ = upOneAndSome' 3 9
-- Result will be `13`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- `let` and `where` declarations can also include type signatures.

upOne'' n =
  let one :: Integer
      one = 1
  in n + one
_ = upOne'' 23
-- Result will be `24`.

upOne''' n = n + one
  where one :: Integer
        one = 1
_ = upOne''' 23
-- Result will be `24`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Keyword: A term used as a programming language construct.
-- Let expression: An expression with declarations introduced into it.
-- Scope (visibility): Area of a program where a variable's binding applies.
