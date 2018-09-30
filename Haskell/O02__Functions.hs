module O02__Functions where

-- A function is an instruction for producing an output from an input argument.

-- Functions are applied to arguments which binds their parameters to values.

-- The fully applied function with its arguments is then evaluated to produce
-- the output or result.

-- Here we declare a function named `double` that accepts a formal parameter
-- named `x` which gets used in the body of the function to calculate the
-- return value which in this case is the doubling of `x`.

double x = x * 2

-- Function names begin with a lowercase letter.

-- Most functions in Haskell are pure, meaning they will only produce the same
-- output when applied to the same input.

_ = double 3    -- `6`

-- An application of a pure function can be replaced by the function's body and
-- still get the same result.

_ = 3 * 2   -- `6`

-- Functions that are not pure are covered later.

-- When a function is applied to an argument, the value of the argument is
-- bound, or unified, with the named parameter in our function definition.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Here, the function `addAndMultiply` takes parameters `x` and `y`, and returns
-- the result of a calculation using them.

addAndMultiply x y = x + y * (x + y)

-- `addAndMultiply` has the appearance of a function that accepts two arguments,
-- and indeed can be used that way.

_ = addAndMultiply 4 5    -- `49`

-- However, all Haskell functions that appear to accept multiple values are
-- really a series of nested functions that each accepts one value and
-- returns one result.  Haskell functions are "curried" by default.

-- Therefore, in Haskell any function that appears to take more than one
-- argument is a higher-order function.

-- We can partially apply our declared function to access the implied nested
-- function within it.

c = addAndMultiply 4
-- Result is a function similar to `addAndMultiply` but lacking the `x`
-- parameter and having the value `4` exist where instances of the `x`
-- parameter originally were.

_ = c 5   -- `49`

-- As with any other function application, `c` can be replaced by its body.

_ = (addAndMultiply 4) 5    -- `49`

-- Functions appearing to have multiple parameters can be partially applied due
-- to currying.

addAndMultiplyMore x y z1 z2 = x + y * (z1 + z2)
_ = addAndMultiplyMore 2 5 8 11         -- `97`
_ = (addAndMultiplyMore 2) 5 8 11       -- `97`
_ = (addAndMultiplyMore 2 5) 8 11       -- `97`
_ = ((addAndMultiplyMore 2) 5) 8 11     -- `97`
_ = (addAndMultiplyMore 2 5 8) 11       -- `97`
_ = ((addAndMultiplyMore 2) 5 8) 11     -- `97`
_ = ((addAndMultiplyMore 2 5) 8) 11     -- `97`
_ = (((addAndMultiplyMore 2) 5) 8) 11   -- `97`
_ = (((addAndMultiplyMore 2) 5) 8) 11   -- `97`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- A partial function (not to be confused with a partially applied function) is
-- a function that doesn't handle all possible inputs.

foo 1 = 10
foo 2 = 99

_ = foo 1   -- `10`
_ = foo 2   -- `99`
_ = foo 3   -- Throws an exception.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Functions without names are anonymous and they are expressed using lambda
-- abstraction syntax.

triple :: Integer -> Integer
triple x = x * 3

_ = triple 5    -- `15`

_ = (\x -> x * 3) :: Integer -> Integer

_ = (\x -> x * 3) 5   -- `15`

-- Parentheses around an anonymous function are required if it gets applied to
-- its argument direcly.

{-
_ = \x -> x * 3 5
-}

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Equivalent function declarations can vary greatly with lambda syntax.

f1 x y z = x * y * z
f2 x y   = \z -> x * y * z
f3 x     = \y z -> x * y * z
f4 x     = \y -> \z -> x * y * z
f5       = \x y z -> x * y * z
f6       = \x y -> \z -> x * y * z
f7       = \x -> \y z -> x * y * z
f8       = \x -> \y -> \z -> x * y * z

_ = f1 3 4 5    -- `60`
_ = f2 3 4 5    -- `60`
_ = f3 3 4 5    -- `60`
_ = f4 3 4 5    -- `60`
_ = f5 3 4 5    -- `60`
_ = f6 3 4 5    -- `60`
_ = f7 3 4 5    -- `60`
_ = f8 3 4 5    -- `60`

-- -----------------------------------------------------------------------------

-- Anonymous functions are most useful as arguments to a higher-order function.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- "Point-free" style (a.k.a. tacit programming) is a way of declaring functions
-- where the parameters of a function, the "points", are not explicitly used.

-- not pointfree
blah x = x
addAndDrop x y = x + 1
reverseMkTuple a b = (b, a)
reverseTuple (a, b) = (b, a)

-- pointfree versions of the above
blah' = id
addAndDrop' = const . (1 +)
reverseMkTuple' = flip (,)
reverseTuple' = uncurry (flip (,))

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Function: An expression that accepts an argument and returns a result.
-- Argument: An input value to the function applied to it.
-- Parameter (formal parameter): A variable for an argument to a function.
-- Return value: The result of a function evaluation.

-- Function application: The giving of an argument to a function for evaluation.
-- Function body: The part of the function which gets evaluated.

-- Named function: A bound function.
-- Function name: The identifier for a function to use during application.

-- Pure function: A function producing the same output given the same input.
-- Currying: Applying nested functions to a series of corresponding arguments.
-- Partial application: Passing some but not all arguments to a function.

-- Composition: Function application on result of another function application.

-- First-class value: A value that can be used as an argument to a function.

-- Anonymous function: A function without a name, therefore unbound.

-- Higher-order function: A function that accepts and/or returns a function.

-- Point-free: A style of function declaration without writing the parameters.
-- Point: A function parameter.
