module O09_Comparison where

x :: Integer
x = 9

-- The equality operator, `==`, checks if a couple expressions are equal.

_ = x == 5
-- Result is `False`.

_ = x == 9
-- Result is `True`.

_ = x == 10
-- Result is `False`.

-- The inequality operator, `/=`, checks if a couple expressions are not equal.

_ = x /= 5
-- Result is `True`.

_ = x /= 9
-- Result is `False`.

_ = x /= 10
-- Result is `True`.

-- The less-than operator, `<`, checks if an expression is lesser than another.

_ = x < 5
-- Result is `False`.

_ = x < 9
-- Result is `False`.

_ = x < 10
-- Result is `True`.

-- The less-than-or-equal-to operator, `<=`, checks if an expression is
-- lesser than or equal to another.

_ = x <= 5
-- Result is `False`.

_ = x <= 9
-- Result is `True`.

_ = x <= 10
-- Result is `True`.

-- The greater-than operator, `>`, checks if an expression is greater than another.

_ = x > 5
-- Result is `True`.

_ = x > 9
-- Result is `False`.

_ = x > 10
-- Result is `False`.

-- The greater-than-or-equal-to operator, `>=`, checks if an expression is
-- greater than or equal to another.

_ = x >= 5
-- Result is `True`.

_ = x >= 9
-- Result is `True`.

_ = x >= 10
-- Result is `False`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Running `:t (==)` in GHCi will show that the equality operator has
-- a typeclass constraint using the `Eq` typeclass.
--
--     (==) :: Eq a => a -> a -> Bool
--

-- The `Eq` typeclass provides the `==` and `/=` operators.

-- Running `:t (<)` in GHCi will show that the less-than operator has
-- a typeclass constraint using the `Ord` typeclass.
--
--     (<) :: Ord a => a -> a -> Bool
--

-- The `Ord` typeclass provides the `<`, `<=`, `>`, and `>=` operators,
-- and some other functions.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Other types can be compared as well.

_ = 'a' == 'a'
-- Result is `True`.

_ = 'a' == 'b'
-- Result is `False`.

_ = 'a' < 'b'
-- Result is `True`.

_ = 'a' > 'b'
-- Result is `False`.

_ = 'a' == 'A'
-- Result is `False`.

_ = "Hello" == "Hello"
-- Result is `True`.

_ = "Hello" == "hello"
-- Result is `False`.

_ = "Hello" == "World"
-- Result is `False`.

_ = "Hello" < "hello"
-- Result is `True`.

_ = "Hello" > "hello"
-- Result is `False`.

_ = "Hello" < "World"
-- Result is `True`.

_ = "Hello" > "World"
-- Result is `False`.

-- Attempting to compare values having different types will cause an error.
--
--     'a' == 8
--

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The boolean conjunction, or logical AND, operator `&&` returns `True` if both
-- operands are `True`.

_ = True && True
-- Result is `True`.

_ = not (True && True)
-- Result is `False`.

_ = (8 > 4) && (4 > 5)
-- Result is `False`.

-- The boolean disjunction, or logical OR, operator `||` returns `True` if
-- either operands are `True`.

_ = False || True
-- Result is `True`.

_ = (8 > 4) || (4 > 5)
-- Result is `True`.

_ = not ((8 > 4) || (4 > 5))
-- Result is `False`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- An "if" expression determines what gets evaluated depending on a given
-- boolean condition.

_ = if True then "Yes" else "No"
-- Result is `"Yes"`.

_ = if False then "Yes" else "No"
-- Result is `"No"`.

-- If the conditional is not a boolean, an error will occur.
--
--     if "A" then "Yes" else "No"
--

-- If the expression in the `then` branch has a different type than the
-- expression in the `else` branch, an error will occur.
--
--     if True then "Yes" else 0
--
