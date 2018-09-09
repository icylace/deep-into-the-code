module O05__SignificantWhitespace where

-- Indentation helps determine the meaning of code.

-- Functions, parameters, and arguments must be separated from one another
-- with spacing.

f' x y = div x y

-- However, spaces around operators are optional.

_ = 2 ^ 9   -- `512`
_ = 2^ 9    -- `512`
_ = 2 ^9    -- `512`
_ = 2^9     -- `512`
_ = -3      -- `-3`
_ = - 3     -- `-3`

-- Expressions can get large enough to merit spanning it across multiple lines
-- to maintain readbility.  Lines that are continuations of an expression
-- should be indented under the beginning of that expression.

f x = 1 + (x * 2) + (x * 3)
    + (x * 4) + (x * 5) + (x * 6)
    + (x * 7) + (x * 8) + (x * 9)

-- All top-level declarations in the same source code file must begin at the
-- same column.  This column is determined by where the first declaration
-- begins on its line.

-- It is good practice to not have any whitespace precede declarations.

-- Improper spacing will lead to errors.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Significant whitespace: Whitespace that helps determine meaning of code.
