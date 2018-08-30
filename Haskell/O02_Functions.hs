module O02_Functions where

-- Here we declare a function named `double` that accepts a formal parameter
-- named `x` which gets used in the body of the function to calculate the
-- return value which in this case is the doubling of `x`.

double x = x * 2

-- Function names begin with a lowercase letter.

-- Most functions in Haskell are pure, meaning they will produce the same
-- output when applied to the same input.

_ = double 3
-- Result is `6`.

-- An application of a pure function can be replaced by the function's body
-- and still get the same result.

_ = 3 * 2
-- Result is `6`.

-- Functions that are not pure are covered later.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Here, the function `addAndMultiply` takes parameters `x` and `y`, and
-- returns the result of a calculation using them.

addAndMultiply x y = x + y * (x + y)

-- `addAndMultiply` has the appearance of a function that accepts two arguments,
-- and indeed can be used that way.

_ = addAndMultiply 4 5
-- Result is `49`.

-- However, technically speaking, all functions in Haskell have precisely one
-- parameter and always return a value.  So, the above expression is actually
-- applying a series of nested functions to a series of arguments to those
-- functions.  This process is known as currying.

-- We can partially apply our declared function to access the implied nested
-- function within it.

c = addAndMultiply 4
-- Result is a function similar to `addAndMultiply` but lacking the `x`
-- parameter and having the value `4` exist where instances of the `x`
-- parameter originally were.

_ = c 5
-- Result is `49`.

-- As with any other function application, `c` can be replaced by its body.

_ = (addAndMultiply 4) 5
-- Result is `49`.

-- Functions appearing to have multiple parameters can be "broken up"
-- due to currying.

addAndMultiplyMore x y z1 z2 = x + y * (z1 + z2)
_ = addAndMultiplyMore 2 5 8 11
_ = (addAndMultiplyMore 2) 5 8 11
_ = (addAndMultiplyMore 2 5) 8 11
_ = ((addAndMultiplyMore 2) 5) 8 11
_ = (addAndMultiplyMore 2 5 8) 11
_ = ((addAndMultiplyMore 2) 5 8) 11
_ = ((addAndMultiplyMore 2 5) 8) 11
_ = (((addAndMultiplyMore 2) 5) 8) 11
_ = (((addAndMultiplyMore 2) 5) 8) 11
-- All nine results are `97`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- A partial function is a function that doesn't handle all possible cases.

foo 1 = 10
foo 2 = 99

_ = foo 1
-- Result is `10`.

_ = foo 2
-- Result is `99`.

_ = foo 3
-- Result is a runtime exception.

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
-- Partial application: Fixing some arguments to get a function for the rest.

-- Partial function: A function that does not handle all possible inputs.
