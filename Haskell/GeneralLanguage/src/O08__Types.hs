module O08__Types () where

-- We import concrete integer types to use later.

import GHC.Int

-- A type, or data type, is a classification of values. Every value has an
-- associated type.

-- The compiler can determine the type for a value automatically through a
-- process called type inference.

x = 'b'
-- `x` has the type `Char`.

-- The value's type can be explicitly written which is good for maintainability.

y :: Char
y = 'b'
-- `y` has the type `Char`.














-- Haskell comes with several types.



-- TODO: use the newer notation style for GHCi stuff here?

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
-- characters. The brackets around `Char` is syntactic sugar for a list.

-- `String` is a type alias, a convenient synonym, for a list of characters.





hi :: String
hi = "Hi!"





-- Explicitly writing out type signatures is optional because the Haskell
-- compiler can automatically infer what a value's type will be.

-- However, it is considered best-practice to use explicit type signatures
-- especially for larger programs.







-- A type alias is an alternative name for a preexisting type that usually
-- communicates something more specific or more briefly.

type Name = String













-- A function is polymorphic when its type signature has variables that can
-- represent more than one type.
















-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The following data declaration is similar to the Boolean type that comes with
-- Haskell.  `MyBool` is the type constructor, the name of the type.  `MyFalse`
-- and `MyTrue` are data constructors which create values of the type.  The
-- pipe symbol, `|`, denotes a sum type which is a type composed of
-- distinct values.

data MyBool = MyFalse | MyTrue

-- A data declaration always creates a new type constructor but may or may not
-- create a new data constructor.

-- `MyFalse` and `MyTrue` were data constructors that took no arguments, so they
-- are called nullary constructors.



-- A function can behave differently depending upon the particular value, or
-- pattern, it is applied to.  This pairing of particular values with
-- particular behaviors is known as pattern matching.

yesNo :: MyBool -> String
yesNo MyTrue  = "Yes"
yesNo MyFalse = "No"

_ = yesNo MyTrue    -- `"Yes"`
_ = yesNo MyFalse   -- `"No"`



-- Data contructors are functions that create values of a certain type.
data Pet = Cat String | DogWithAge String Integer

_ = Cat "Foo"   -- `Cat "Foo"`
-- `Cat` has the type `String -> Pet`.

_ = DogWithAge "Bar" 2    -- `DogWithAge "Bar" 2`
-- `DogWithAge` has the type `String -> Integer -> Pet`.




-- Data constructors which take no arguments are nullary and they represent
-- constant values.

data OnlyMe = OnlyMe




-- Data types that don't have data constructors are only useful
-- in type signatures.

data NotHere


-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- A function can pattern match on a "don't care" parameter (represented by `_`)
-- which will match on anything.

sillyIncrease :: Int -> Int
sillyIncrease 8  = 1
sillyIncrease 10 = 2
sillyIncrease _  = 3

_ = sillyIncrease 8     -- `1`
_ = sillyIncrease 10    -- `2`
_ = sillyIncrease 1     -- `3`
_ = sillyIncrease 2     -- `3`
_ = sillyIncrease 9     -- `3`







-- Values that could have one type or another given its context can have their
-- type explicitly given.

_ = 1   -- `1`

_ = 1 :: Double   -- `1.0`













-- Declaring type signatures for expressions are optional because Haskell has
-- type inference which is an algorithm for determining an expression's type.

-- Haskell will infer the most generally applicable type that is still correct.

hey x = "hey " ++ x
-- `hey` has the type `[Char] -> [Char]`.

smoosh x y = x ++ y
-- `smoosh` has the type `[a] -> [a] -> [a]`.






















-- The term level is where values live and code execution happens.

-- The type level is where types live and static analysis and program
-- verification happens.












-- Haskell conventions for variables (according to section 4.11 of "Haskell
-- Programming from First Principles"):
--
-- Type variables generally start at `a` and each additional type variable is
-- assigned the next available letter of the alphabet.
--
-- Type variables may also be numbered (e.g. `a1`).
--
-- Variables that represent functions typically start at `f` with each
-- additional function variable being assigned the next available
-- letter of the alphabet.
--
-- Function variables may also be numbered (e.g. `f1`).
--
-- Function variables may also be decorated with apostrophes (e.g. `f'`,
-- pronounced "eff-prime").  "Prime" functions are typically closely
-- related in some way to their "non-prime" counterparts.
--
-- Function variables may also be named in a more descriptive way depending
-- on the context.
--
-- Variables in smaller programs are often single-lettered.
--
-- Variables in larger programs are often more self-descriptive.
--
-- Variables in domain-specific code should use domain-specific names.
--
-- Function arguments typically start at `x` and may be numbered (e.g. `x1`).
--
-- Other single-letter variable names may be chosen when they serve mnemonic
-- role (e.g. `r` for circle radius).
--
-- Names of lists are pluralized (e.g. a list whose items are `x` would
-- be named `xs`).























-- A typeclass constraint consists of a type variable for a type which has an
-- instance of a given typeclass.

{- GHCi ------------------------------------------------------------------------
> :t (/)
(/) :: Fractional a => a -> a -> a
-------------------------------------------------------------------------------}

-- `Fractional` requires types to already have an instance of the `Num`
-- typeclass.  `Num` is a superclass of `Fractional`.

-- This means that types implementing `Fractional` can use functions from both
-- `Fractional` and `Num`.

-- However, types implementing just `Num` but not also `Fractional` can use
-- functions from `Num` but not `Fractional`.

-- Using integer numbers with the division operator will result in a number that
-- is fractional.

_ = 4 / 2   -- `2.0`

-- Values of the type `Fractional a => a` default to the `Double` type.

-- Explicitly using the `Double` type is less desirable than explicitly using
-- the `Scientific` type due to the latter's arbitrary precision and thus
-- fewer quirks.

-- TODO:
-- include example comparing Double and Scientific









-- A type signature can make use of multiple typeclass constraints.

foo1 :: (Num a, Num b) => a -> b -> b
foo1 = undefined

foo2 :: (Num a, Num a) => a -> a -> a
foo2 = undefined







-- Parametric polymorphism is when a type variable is not contrained by a
-- typeclass.

-- `id` is a parametric polymorphic function having the type `a -> a` that
-- returns its argument.

_ = id 3                -- `3`
_ = id "Hi"             -- `"Hi"`
_ = id True             -- `True`
_ = (id head) [1, 2]    -- `1`
_ = id id 3             -- `3`
_ = id . id $ 3         -- `3`
_ = id $ id $ 3         -- `3`
_ = id $ id 3           -- `3`










-- GHCi can give the type of a given value.

{- GHCi ------------------------------------------------------------------------
> :t True
True :: Bool

> :t 1.0
1.0 :: Fractional p => p

> :t "Hello"
"Hello" :: [Char]
-------------------------------------------------------------------------------}












-- In Haskell it is impossible to create untyped data.  Aside from some
-- syntactic sugar for things like numbers or functions, everything
-- originates in a data constructor from some type definition.















-- A datatype is how we declare and create data for our functions to receive as
-- inputs.  Datatype declarations begin with the keyword `data`.  A datatype is
-- made up of a type constructor and zero or more data constructors which each
-- have zero or more arguments.


















-- Numbers in and of themselves don't have a concrete type.

{- GHCi ------------------------------------------------------------------------
> :t 13
13 :: Num p => p

> :t 13.0
13.0 :: Fractional p => p
-------------------------------------------------------------------------------}

-- A concrete type can be chosen by declaring it.

_ = 13 :: Integer















-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Haskell provides several numeric types.

-- `Integral` types:

-- The `Int` type represents integers and is fixed-precision, meaning it has a
-- defined minimum and defined maximum.

-- The `Integer` type is similar to `Int` except that is is arbitrary-precision,
-- meaning it does not have a fixed range.

-- `Fractional` types:

-- The `Float` type represents single-precision floating-point numbers and is
-- best suited for graphics programming such as with OpenGL.

-- The `Double` type represents double-precision floating-point numbers.  It
-- uses twice as many bits as `Float` to represent floating-point numbers
-- which grants it the ability to represent a wider range of numbers
-- than `Float`.

-- Fixed-point numbers have a specific maximum potential number of digits before
-- and after the decimal point.  Floating-point numbers have a certain amount of
-- flexibility in shifting the maximum potential digits before and after the
-- decimal point.  However, this flexibility comes at the cost of a certain
-- degree of accuracy.

-- The `Rational` type has arbitrary-precision and represents a ratio between
-- a couple integers.

_ = 1 / 2 :: Rational             -- `1 % 2`
_ = 1.5 / 2.5 :: Rational         -- `3 % 5`
_ = 34.56 / 123.345 :: Rational   -- `768 % 2741`

-- The `Scientific` type has arbitrary-precision and represents a number in
-- scientific notation.  It stores the coefficient as an `Integer` and the
-- exponent as an `Int`.  `Scientific` is more efficient than `Rational`
-- and is available as a library.

-- Arbitrary precision number types can be used for calculations that require a
-- high degree of precision.






-- These numeric types have instances of the `Num` typeclass.

-- A typeclass is a way of defining values and functions that are reusable
-- across any type that has an instance of that typeclass.

-- In Haskell, typeclasses are unique pairings of class and concrete instance.
-- For example, if a given type has an instance of `Num` then it has only one
-- instance of `Num`.

-- The `Num` typeclass provides common numerical operators like `+`, `-`, and
-- `*`, as well as a few other functions.







-- Integral numbers have no fractional component.




-- The `Int8` concrete type can represent integers in the range between -128 and
-- 127 inclusively.  The "8" in `Int8` refers to the number of bits used to
-- represent its values.

_ = 127 :: Int8   -- `127`

-- Out-of-bound numbers will be "looped" until it falls within range.

_ = (127 + 10) :: Int8              -- `-119`
_ = (127 :: Int8) + 10              -- `-119`
_ = 127 + (10 :: Int8)              -- `-119`
_ = (127 :: Int8) + (10 :: Int8)    -- `-119`

-- A warning will be given for values that are out of range.

_ = 100000 :: Int8    -- `-96`
-- Triggers a warning when the `-Woverflowed-literals` compiler option is used.
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#ghc-flag--Woverflowed-literals

_ = -100000 :: Int8   -- Results in `96` with a warning.
-- Triggers a warning when the `-Woverflowed-literals` compiler option is used.
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#ghc-flag--Woverflowed-literals


-- TODO: explain why for the following
-- Usually, `Integer` is preferred over other integral types.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The `Bounded` typeclass provides `maxBound` and `minBound` which show the
-- limit values of types.

_ = minBound :: Int8    -- `-128`
_ = maxBound :: Int8    -- `127`
_ = minBound :: Int16   -- `-32768`
_ = maxBound :: Int16   -- `32767`
_ = minBound :: Int32   -- `-2147483648`
_ = maxBound :: Int32   -- `2147483647`
_ = minBound :: Int64   -- `-9223372036854775808`
_ = maxBound :: Int64   -- `9223372036854775807`
_ = minBound :: Int     -- `-9223372036854775808`
_ = maxBound :: Int     -- `9223372036854775807`

-- Non-numeric types can also define minimum and maximum bounds.

_ = minBound :: Bool    -- `False`
_ = maxBound :: Bool    -- `True`
_ = minBound :: Char    -- `'\NUL'`
_ = maxBound :: Char    -- `'\1114111'`
_ = minBound :: ()      -- `()`
_ = maxBound :: ()      -- `()`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

Type (datatype): A classification of values that share something in common.
Type alias: An alternative name for a preexisting type.
Type signature: The definition of types used by an expression.

Constructor: A means, possibly parameterized, to create a type or a value.
Constant: A constructor that is not parameterized.

Type constructor: A constructor that creates a type.
Concrete type: A type constructor that is not parameterized.
Type constant: A concrete type that takes no arguments.

Data constructor: A constructor that creates values of a certain type.
Constant value: A data constructor that takes no arguments.

Data declaration: A declaration that defines a new datatype.

Sum type: A type where each value is from a specific type of a set of types.
Sum type: A type having multiple data constructors inhabiting it.

Product type: A type made of a set of types compounded over each other.

Phantom type: A parameterized type that does not use all of its parameters.


Nullary: Taking no arguments.



Type constructor: The name of a type which is used in type signatures.


Type level: The areas of a program that relate to type signatures.
Term level: The areas of a program that relate to values.
Type inference: The automatic detection of a value's type.



Type argument:




Typecheck: To validate types at compile-time.




Pattern: Syntax that matches against particular values.
Pattern matching: Pairing specific function behavior with specific patterns.

Compound type: A type made out of basic types and other compound types.

Concrete type: A type whose name references how its values are stored.
Monomorphic restriction: Top-level bindings have concrete type when possible.

Static typing: The checking of types at compile time.

Actual type: The type actually provided in the code.
Expected type: The type the compiler expected to be given in the code.

Library: A collection of functions related in some way.

-}
