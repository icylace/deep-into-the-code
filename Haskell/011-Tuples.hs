module Tuples where

-- The `Data.Tuple` provides the `swap` function.
import Data.Tuple

-- A tuple is a type whose values are groups of a certain fixed-size
-- that contain other values having the same or different types.

_ = (,,,) 8 "Hello" "World" 'c'
_ = (8, "Hello", "World", 'c')
-- Both results will be `(8,"Hello","World",'c')`.

-- A triple is a tuple containing three other values.

_ = (,,) 8 "Hello" "World"
_ = (8, "Hello", "World")
-- Both results will be `(8,"Hello","World")`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- A pair is a tuple containing a couple other values.

_ = (,) 8 10
_ = (8, 10)
-- Both results will be `(8,10)`.

_ = (,) 8 "Hello"
_ = (8, "Hello")
-- Both results will be `(8,"Hello")`.

-- The `fst` function gets the left value of a pair.

_ = fst (8, "Hello")
-- Result will be `8`.

_ = 1 + fst (8, "Hello")
-- Result will be `9`.

-- The `snd` function gets the right value of a pair.

_ = snd (8, "Hello")
-- Result will be `"Hello"`.

_ = snd (8, "Hello") ++ " World"
-- Result will be `"Hello World"`.

-- The `swap` function exchanges the left and right values with each other.

_ = swap (8, "Hello")
-- Result will be `("Hello",8)`.

_ = swap $ swap (8, "Hello")
-- Result will be `(8,"Hello")`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- A tuple must contain more than a single value.  Attempting to use a tuple
-- without values or with only a single value will cause an error.
--
--     (,)
--     (,) 8
--

-- Larger tuples are less efficient than smaller ones.






-- Key Terms
-- =========
-- Product type: A type that is a conjunction of two or more constituent types.
-- Tuple: A product type whose values are fixed groups of other values.
-- Arity: The number of values contained within a tuple.
-- Pair (two-tuple): A tuple whose values contain two other values.
-- Triple (three-tuple): A tuple whose values contain three other values.




-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

main :: IO ()
main = print ()
