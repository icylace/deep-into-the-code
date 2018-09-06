module O09__Comparison where

-- The boolean conjunction, or logical AND, operator `&&` returns `True` if both
-- operands are `True`.

_ = True && True          -- `True`
_ = not (True && True)    -- `False`
_ = (8 > 4) && (4 > 5)    -- `False`

-- The boolean disjunction, or logical OR, operator `||` returns `True` if
-- either operands are `True`.

_ = False || True               -- `True`
_ = (8 > 4) || (4 > 5)          -- `True`
_ = not ((8 > 4) || (4 > 5))    -- `False`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- An "if" expression determines what gets evaluated depending on a given
-- boolean condition.

_ = if True then "Yes" else "No"    -- `"Yes"`
_ = if False then "Yes" else "No"   -- `"No"`

-- If the conditional is not a boolean, an error will occur.

{-
_ = if "A" then "Yes" else "No"
-}

-- If the expression in the `then` branch has a different type than the
-- expression in the `else` branch, an error will occur.

{-
_ = if True then "Yes" else 0
-}
