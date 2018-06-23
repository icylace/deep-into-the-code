module BasicsOfTypes where

-- We import concrete integer types to be used later.

import GHC.Int

-- A type, or data type, is a classification of values.  Every value has an
-- associated type.

-- Haskell comes with several types.




-- The GHCi command `:t` (`:type`) displays the type of a value.

-- For example:
--
--     :t 'a'
--
-- The result will be:
--
--     'a' :: Char
--
-- This means that the single letter `a`, a character, has the type of `Char`.

-- For example:
--
--     :t "Hi!"
--
-- The result will be:
--
--     "Hi!" :: [Char]
--
-- This means that the sequence of characters `Hi!` has the type of list of
-- characters.  The brackets around `Char` is syntactic sugar for a list.

-- `String` is a type alias, a convenient synonym, for a list of characters.





hi :: String
hi = "Hi!"





-- Explicitly writing out type signatures is optional because the Haskell
-- compiler can automatically infer what a value's type will be.

-- However, it is considered best-practice to use explicit type signatures
-- especially for larger programs.










-- Lists of the same type can be concatenated together.

_ = ['a'] ++ ['b']
_ = ['a'] ++ "b"
_ = "a" ++ ['b']
_ = "a" ++ "b"
-- All four results will be `ab`.

_ = [1] ++ [2]
-- Result will be `[1, 2]`.

-- Attempting to concatenate lists of different types will cause an error.
--
--     ['a'] ++ [2]
--     "a" ++ [2]
--     [1] ++ ['b']
--     [1] ++ "b"
--

-- The append operator, `++`, is an example of a polymorphic function meaning
-- it can work with different types as long as those types are used with
-- it correctly.






-- Type constructor: The name of a type which is used in type signatures.






-- The function `not` takes a `Bool` value and returns the other `Bool` value.

_ = not False
-- Result will be `True`.

_ = not True
-- Result will be `False`.

-- This data declaration is similar to the Boolean type that comes with Haskell.
-- `MyBool` is the type constructor, the name of the type.  `False` and `True`
-- are data constructors which are values of the type.  The pipe symbol,
-- `|`, denotes a sum type which is a type composed of distinct values.

data MyBool = MyFalse | MyTrue

-- A function can behave differently depending upon the particular value,
-- or pattern, it is applied to.  This pairing of particular values with
-- particular behaviors is known as pattern matching.

yesNo :: MyBool -> String
yesNo MyTrue  = "Yes"
yesNo MyFalse = "No"

_ = yesNo MyTrue
-- Result will be `"Yes"`.

_ = yesNo MyFalse
-- Result will be `"No"`.

-- A function can pattern match on a "don't care" parameter (represented by `_`)
-- which will match on anything.

sillyIncrease :: Int -> Int
sillyIncrease 8  = 1
sillyIncrease 10 = 2
sillyIncrease _  = 3

_ = sillyIncrease 8
-- Result will be `1`.

_ = sillyIncrease 10
-- Result will be `2`.

_ = sillyIncrease 1
_ = sillyIncrease 2
_ = sillyIncrease 9
-- All three results will be `3`.







-- Values that could have one type or another given its context can have their
-- type explicitly given.

_ = 1
-- Result will be `1`.

_ = 1 :: Double
-- Result will be `1.0`.










-- A type variable is a placeholder for a type.






-- When a type implements a typeclass it must have a declaration for every
-- function provided in the typeclass.











-- A type that implements a typeclass is said to have an instance of
-- the typeclass.





-- A typeclass constraint consists of a type variable for a type which has
-- an instance of a given typeclass.

-- Running `:t (/)` in GHCi will show that the division operator has a typeclass
-- constraint using the `Fractional` typeclass.
--
--     (/) :: Fractional a => a -> a -> a
--

-- `Fractional` requires types to already have an instance of the `Num`
-- typeclass.  `Num` is a superclass of `Fractional`.

-- This means that types implementing `Fractional` can use functions from both
-- `Fractional` and `Num`.

-- However, types implementing just `Num` but not also `Fractional` can use
-- functions from `Num` but not `Fractional`.

-- Using integer numbers with the division operator will result in a number
-- that is fractional.

_ = 4 / 2
-- Result will be: 2.0

-- Values of the type `Fractional a => a` default to the `Double` type.

-- Explicitly using the `Double` type is less desirable than explicitly using
-- the `Scientific` type due to the latter's arbitrary precision and thus
-- fewer quirks.

-- TODO:
-- include example comparing Double and Scientific












-- GHCi can give the type of a given value.

-- Running `:t True` in GHCi will result in `True :: Bool`.

-- Running `:t 1.0` in GHCi will result in `1.0 :: Fractional p => p`.

-- Running `:t "Hello"` in GHCi will result in `"Hello" :: [Char]`.
















-- Key Terms
-- =========
-- Type (datatype): A classification of values that share something in common.
-- Type alias: An alternative name for a preexisting type.
-- Type signature: The definition of types used by an expression.
-- Type variable: A placeholder for a type.
-- Type constructor: A function that accepts a type and returns a type.
-- Data constructor: A value that inhabits the type it is defined in.
-- Data declaration: A declaration that defines a type.

-- Sum type: A type composed of distinct values.

-- Type level: The areas of a program that relate to type signatures.
-- Term level: The areas of a program that relate to values.
-- Type inference: The automatic detection of a value's type.
-- Typeclass: A set of functions that can be shared across sets of types.
-- Typeclass constraint: A type variable for a type implementing a typeclass.
-- Polymorphism: The usage of an function across different sets of types.
-- Superclass: A typeclass required by another typeclass.

-- Typeclass instance (instance): An implementation of a typeclass for a type.

-- Pattern: Syntax that matches against particular values.
-- Pattern matching: Pairing specific function behavior with specific patterns.

-- Concrete type: A type whose name references how its values are stored.






-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Haskell provides several numeric types.

-- `Integral` types:

-- The `Int` type represents integers and is fixed-precision, meaning it has
-- a defined minimum and defined maximum.

-- The `Integer` type is similar to `Int` except that is is arbitrary-precision,
-- meaning it does not have a fixed range.

-- `Fractional` types:

-- The `Float` type represents single-precision floating-point numbers and is
-- best suited for graphics programming such as with OpenGL.

-- The `Double` type represents double-precision floating-point numbers.
-- It uses twice as many bits as `Float` to represent floating-point
-- numbers which grants it the ability to represent a wider range
-- of numbers than `Float`.

-- Fixed-point numbers have a specific maximum potential number of digits before
-- and after the decimal point.  Floating-point numbers have a certain amount of
-- flexibility in shifting the maximum potential digits before and after the
-- decimal point.  However, this flexibility comes at the cost of a certain
-- degree of accuracy.

-- The `Rational` type has arbitrary-precision and represents a ratio between
-- a couple integers.

_ = 1 / 2 :: Rational
-- Result will be `1 % 2`.

_ = 1.5 / 2.5 :: Rational
-- Result will be `3 % 5`.

_ = 34.56 / 123.345 :: Rational
-- Result will be `768 % 2741`.

-- The `Scientific` type has arbitrary-precision and represents a number in
-- scientific notation.  It stores the coefficient as an `Integer` and the
-- exponent as an `Int`.  `Scientific` is more efficient than `Rational`
-- and is available as a library.

-- Arbitrary precision number types can be used for calculations that
-- require a high degree of precision.






-- These numeric types have instances of the `Num` typeclass.

-- A typeclass is a way of adding functionality to types that is reusable across
-- any type that has an instance of that typeclass.

-- The `Num` typeclass provides common numerical operators like `+`, `-`, and
-- `*`, as well as a few other functions.







-- Integral numbers have no fractional component.




-- The `Int8` concrete type can represent integers in the range between -128
-- and 127 inclusively.  The "8" in `Int8` refers to the number of bits used
-- to represent its values.

_ = 127 :: Int8
-- Result will be `127`.

-- Out-of-bound numbers will be "looped" until it falls within the `Int8` range.

_ = (127 + 10) :: Int8
_ = (127 :: Int8) + 10
_ = 127 + (10 :: Int8)
-- All three results will be `-119`.

-- A warning will be given for values that are outside the range of `Int8`.

_ = 100000 :: Int8
-- Result will be a warning and `-96`.

_ = -100000 :: Int8
-- Result will be a warning and `96`.

-- Usually, `Integer` is preferred over other integral types.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The `Bounded` typeclass provides `maxBound` and `minBound` which show the
-- limit values of types.

_ = minBound :: Int8
-- Result will be `-128`.

_ = maxBound :: Int8
-- Result will be `127`.

_ = minBound :: Int16
-- Result will be `-32768`.

_ = maxBound :: Int16
-- Result will be `32767`.

_ = minBound :: Int32
-- Result will be `-2147483648`.

_ = maxBound :: Int32
-- Result will be `2147483647`.

_ = minBound :: Int64
-- Result will be `-9223372036854775808`.

_ = maxBound :: Int64
-- Result will be `9223372036854775807`.

_ = minBound :: Int
-- Result will be `-9223372036854775808`.

_ = maxBound :: Int
-- Result will be `9223372036854775807`.

-- Non-numeric types can also define minimum and maximum bounds.

_ = minBound :: Bool
-- Result will be `False`.

_ = maxBound :: Bool
-- Result will be `True`.

_ = minBound :: Char
-- Result will be `'\NUL'`.

_ = maxBound :: Char
-- Result will be `'\1114111'`.

_ = minBound :: ()
-- Result will be `()`.

_ = maxBound :: ()
-- Result will be `()`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------










-- Key Terms
-- =========
-- Library: A collection of functions related in some way.












-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

main :: IO ()
main = print ()
