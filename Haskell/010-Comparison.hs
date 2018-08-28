module Comparison where

x :: Integer
x = 9

-- The equality operator, `==`, checks if a couple expressions are equal.

_ = x == 5
-- Result will be `False`.

_ = x == 9
-- Result will be `True`.

_ = x == 10
-- Result will be `False`.

-- The inequality operator, `/=`, checks if a couple expressions are not equal.

_ = x /= 5
-- Result will be `True`.

_ = x /= 9
-- Result will be `False`.

_ = x /= 10
-- Result will be `True`.

-- The less-than operator, `<`, checks if an expression is lesser than another.

_ = x < 5
-- Result will be `False`.

_ = x < 9
-- Result will be `False`.

_ = x < 10
-- Result will be `True`.

-- The less-than-or-equal-to operator, `<=`, checks if an expression is
-- lesser than or equal to another.

_ = x <= 5
-- Result will be `False`.

_ = x <= 9
-- Result will be `True`.

_ = x <= 10
-- Result will be `True`.

-- The greater-than operator, `>`, checks if an expression is greater than another.

_ = x > 5
-- Result will be `True`.

_ = x > 9
-- Result will be `False`.

_ = x > 10
-- Result will be `False`.

-- The greater-than-or-equal-to operator, `>=`, checks if an expression is
-- greater than or equal to another.

_ = x >= 5
-- Result will be `True`.

_ = x >= 9
-- Result will be `True`.

_ = x >= 10
-- Result will be `False`.

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
-- Result will be `True`.

_ = 'a' == 'b'
-- Result will be `False`.

_ = 'a' < 'b'
-- Result will be `True`.

_ = 'a' > 'b'
-- Result will be `False`.

_ = 'a' == 'A'
-- Result will be `False`.

_ = "Hello" == "Hello"
-- Result will be `True`.

_ = "Hello" == "hello"
-- Result will be `False`.

_ = "Hello" == "World"
-- Result will be `False`.

_ = "Hello" < "hello"
-- Result will be `True`.

_ = "Hello" > "hello"
-- Result will be `False`.

_ = "Hello" < "World"
-- Result will be `True`.

_ = "Hello" > "World"
-- Result will be `False`.

-- Attempting to compare values having different types will cause an error.
--
--     'a' == 8
--

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The boolean conjunction, or logical AND, operator `&&` returns `True` if both
-- operands are `True`.

_ = True && True
-- Result will be `True`.

_ = not (True && True)
-- Result will be `False`.

_ = (8 > 4) && (4 > 5)
-- Result will be `False`.

-- The boolean disjunction, or logical OR, operator `||` returns `True` if
-- either operands are `True`.

_ = False || True
-- Result will be `True`.

_ = (8 > 4) || (4 > 5)
-- Result will be `True`.

_ = not ((8 > 4) || (4 > 5))
-- Result will be `False`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- An "if" expression determines what gets evaluated depending on a given
-- boolean condition.

_ = if True then "Yes" else "No"
-- Result will be `"Yes"`.

_ = if False then "Yes" else "No"
-- Result will be `"No"`.

-- If the conditional is not a boolean, an error will occur.
--
--     if "A" then "Yes" else "No"
--

-- If the expression in the `then` branch has a different type than the
-- expression in the `else` branch, an error will occur.
--
--     if True then "Yes" else 0
--

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

main :: IO ()
main = print ()
