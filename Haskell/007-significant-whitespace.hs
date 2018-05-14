-- Indentation helps determine the meaning of code.

-- Code that is part of an expression should be indented under the beginning
-- of that expression.

-- The body of function `foo` is large enough span multiple lines and as such
-- must be indented.
foo x =
  let y = x * 2
      z = x ^ 2
  in 2 * y * z

-- Functions, parameters, and arguments must be separated from one another
-- with spacing.
foo' x y = div x y

-- However, spaces around operators are optional.
_ = 2 ^ 9
_ = 2^ 9
_ = 2 ^9
_ = 2^9
-- All four results will be `512`.

-- All top-level declarations in the same source code file must begin at the
-- same column.  This column is determined by where the first declaration
-- begins on its line.

-- It is good practice to not have any whitespace precede declarations.

-- Improper spacing will lead to errors.

-- Key Terms
-- =========
-- Significant whitespace: Whitespace that helps determine meaning of code.
