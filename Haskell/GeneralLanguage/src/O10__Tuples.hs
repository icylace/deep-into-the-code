module O10__Tuples () where

-- The `Data.Tuple` module provides the `swap` function.
import Data.Tuple (swap)

-- A tuple is a grouping of fixed-size for values having either the same types
-- or different types.

-- The number of values contained within a tuple is called its arity.

_ = (,,,) 8 "Hello" "World" 'c'    -- `(8, "Hello", "World", 'c')`

-- A tuple can be in syntactic sugar style that may be more familiar.

_ = (8, "Hello", "World", 'c')     -- `(8, "Hello", "World", 'c')`

-- The arity of each of the previous tuples is 4.

-- A triple is a tuple containing three values. Its arity is 3.

_ = (,,) 8 "Hello" "World"    -- `(8, "Hello", "World")`
_ = (8, "Hello", "World")     -- `(8, "Hello", "World")`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- A pair is a tuple containing a couple values. Its arity is 2.

_ = (,) 8 10    -- `(8, 10)`
_ = (8, 10)     -- `(8, 10)`

_ = (,) 8 "Hello"    -- `(8, "Hello")`
_ = (8, "Hello")     -- `(8, "Hello")`

-- `fst` gets the left value of a pair.

_ = fst (8, "Hello")        -- `8`
_ = 1 + fst (8, "Hello")    -- `9`

-- `snd` gets the right value of a pair.

_ = snd (8, "Hello")                -- `"Hello"`
_ = snd (8, "Hello") ++ " World"    -- `"Hello World"`

-- `swap` exchanges the left and right values with each other.

_ = swap (8, "Hello")           -- `("Hello", 8)`
_ = swap $ swap (8, "Hello")    -- `(8, "Hello")`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- In Haskell, a tuple cannot contain just a single value.

-- Attempting to define a single-element tuple ends up being interpreted as a
-- nested expression.

_ = (3)    -- `3`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- A special tuple that contains no values is called unit.

_ = ()    -- `()`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Tuples can be partially applied.

withACouple = (,,,) 8 "Hello"
_ = withACouple 'B' 3.0    -- `(8, "Hello", 'B', 3.0)`

{-
-- The TupleSections language extensions is required to allow sugar syntax for
-- tuples to be partially applied:
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TupleSections

withAnotherCouple = (8, "Hello", )    -- Error.
-}

-- TODO:
-- show how different parts of a tuple can be partially applied using TupleSections:
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#tuplesections

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Tuples are compared on an element-by-element basis.

_ = (8, "Hello") == (8, "Hello")    -- `True`
_ = (8, "Hello") == (8, "World")    -- `False`
_ = (8, "Hello") < (8, "World")     -- `True`
_ = (9, "Hello") < (8, "World")     -- `False`

{-
-- Tuples that are be compared with each other must have the same arity, types,
-- and order of those types.
_ = (8, "Hello") == ("Hello", 8)         -- Error.
_ = (8, "Hello") == (8, 3.0)             -- Error.
_ = (8, "Hello") == ('B', "World")       -- Error.
_ = (8, "Hello") == ('B', 3.0)           -- Error.
_ = (8, "Hello") == (8, "Hello", 'B')    -- Error.
-}

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- There is no limit for how big a tuple's arity can be but very large tuples
-- are not well supported and they're also less efficient than smaller ones.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

Product type: A type that is a conjunction of two or more constituent types.
Tuple: A fixed-length ordered grouping of values.
Tuple type: A product type whose values are tuples.
Arity: The number of values contained within a tuple.
Triple (three-tuple): A tuple whose values contain three other values.
Pair (two-tuple): A tuple whose values contain two other values.
Unit (empty tuple): A tuple that contains no values.

-}
