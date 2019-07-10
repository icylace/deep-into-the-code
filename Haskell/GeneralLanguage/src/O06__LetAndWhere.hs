module O06__LetAndWhere () where

-- A let expression is an expression that has declarations introduced into it.
-- Declarations created in this way are able to be referenced only within the
-- scope of the expression they're associated to.

-- Syntactically, a let expression starts with the `let` keyword, followed by
-- declarations that are being introduced, followed by the `in` keyword, then
-- finally followed by the rest of the expression.

upOne n = let one = 1 in n + one
_ = upOne 23    -- `24`

-- The declarations introduced by `let` can be parameterized.

upSome n m = let some = m in n + some
_ = upSome 3 9    -- `12`

-- Multiple declarations on the same line must be separated with semicolons.

upOneAndSome n m = let one = 1; some = m in n + one + some
_ = upOneAndSome 3 9    -- `13`

-- The `where` keyword has a similar purpose and scoping effect as `let` but
-- it instead defines declarations after its associated expression.

upOne' n = n + one where one = 1
_ = upOne' 23   -- `24`

upSome' n m = n + some where some = m
_ = upSome' 3 9   -- `12`

upOneAndSome' n m = n + one + some where one = 1; some = m
_ = upOneAndSome' 3 9   -- `13`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- `let` and `where` declarations can also include type signatures.

upOne'' n =
  let one :: Integer
      one = 1
  in n + one
_ = upOne'' 23    -- `24`

upOne''' n = n + one
  where one :: Integer
        one = 1
_ = upOne''' 23   -- `24`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- A variable can be shadowed by another variable of the same name at an inner
-- scope.  A reference to a variable will resolve to the variable declaration
-- either at the same scoping level or the next outer scope that's closest to
-- that reference.

-- Here, the `x` parameter is shadowed by the `x` `let`-binding.

bindExp :: Integer -> String
bindExp x =
  let x = 10; y = 5 in
    "the integer was: " ++ show x ++ " and y was: " ++ show y

_ = bindExp 9001    -- `"the integer was: 10 and y was: 5"`

-- -----------------------------------------------------------------------------

-- This example shows how a function argument can shadow a top-level assignment.

x = 5
y = x + 5

_ = y         -- `10`
_ = y * 10    -- `100`

z y = y * 10

_ = x     -- `5`
_ = y     -- `10`
_ = z 9   -- `90`
_ = z y   -- `100`

-- The lexically innermost binding for a variable of a particular name always
-- takes precedence.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

Keyword: A term used as a programming language construct.
Let expression: An expression with declarations introduced into it.
Scope (visibility): Area of a program where a variable's binding applies.
Lexical scoping: Scoping based on location in the code and lexical context.
Shadowing: Variable references resolving to more inner variable declarations.

-}
