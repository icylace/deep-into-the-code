module O10_Tuples where

-- The `Data.Tuple` provides the `swap` function.
import Data.Tuple

-- A tuple is a grouping of fixed-size for values having either the same types
-- or different types.

_ = (,,,) 8 "Hello" "World" 'c'
_ = (8, "Hello", "World", 'c')
-- Both results are `(8,"Hello","World",'c')`.

-- The number of values contained within a tuple is called its arity.
-- The arity of the previous tuple is 4.

-- A triple is a tuple containing three values.  Therefore its arity is 3.

_ = (,,) 8 "Hello" "World"
_ = (8, "Hello", "World")
-- Both results are `(8,"Hello","World")`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- A pair is a tuple containing a couple values.  Its arity is 2.

_ = (,) 8 10
_ = (8, 10)
-- Both results are `(8,10)`.

_ = (,) 8 "Hello"
_ = (8, "Hello")
-- Both results are `(8,"Hello")`.

-- The `fst` function gets the left value of a pair.

_ = fst (8, "Hello")
-- Result is `8`.

_ = 1 + fst (8, "Hello")
-- Result is `9`.

-- The `snd` function gets the right value of a pair.

_ = snd (8, "Hello")
-- Result is `"Hello"`.

_ = snd (8, "Hello") ++ " World"
-- Result is `"Hello World"`.

-- The `swap` function exchanges the left and right values with each other.

_ = swap (8, "Hello")
-- Result is `("Hello",8)`.

_ = swap $ swap (8, "Hello")
-- Result is `(8,"Hello")`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- In Haskell, a tuple cannot contain just a single value.

-- Attempting to define a single-element tuple ends up being interpreted
-- as a nested expression.

_ = (3)
-- Result is `3`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- A special tuple that contains no values is called unit.

_ = ()
-- Result is `()`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Tuples can be partially applied.

withFirstCouple = (,,,) 8 "Hello"
_ = withFirstCouple 'B' 3.0
-- Result is `(8,"Hello",'B',3.0)`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Tuples can be compared on an element-by-element basis.

_ = (8, "Hello") == (8, "Hello")
-- Result is `True`.

_ = (8, "Hello") == (8, "World")
-- Result is `False`.

_ = (8, "Hello") < (8, "World")
-- Result is `True`.

_ = (9, "Hello") < (8, "World")
-- Result is `False`.

-- Tuples that are be compared with each other must have the same arity, types,
-- and order of those types.  Otherwise an error will occur.
--
--     (8, "Hello") == ("Hello", 8)
--     (8, "Hello") == (8, 3.0)
--     (8, "Hello") == ('B', "World")
--     (8, "Hello") == ('B', 3.0)
--     (8, "Hello") == (8, "Hello", 'B')

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- There is no limit for how big a tuple's arity can be but very large tuples
-- are not well supported and they're also less efficient than smaller ones.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Product type: A type that is a conjunction of two or more constituent types.
-- Tuple: A fixed-length ordered grouping of values.
-- Tuple type: A product type whose values are tuples.
-- Arity: The number of values contained within a tuple.
-- Triple (three-tuple): A tuple whose values contain three other values.
-- Pair (two-tuple): A tuple whose values contain two other values.
-- Unit (empty tuple): A tuple that contains no values.
