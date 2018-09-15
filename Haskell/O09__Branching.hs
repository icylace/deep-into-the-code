module O09__Branching where

-- The function `not` takes a `Bool` value and returns the other `Bool` value.

_ = not False   -- `True`
_ = not True    -- `False`






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

-- An `if` expression determines what gets evaluated depending on a given
-- boolean condition.

_ = if True then "Yes" else "No"    -- `"Yes"`
_ = if False then "Yes" else "No"   -- `"No"`

-- `if` expressions can be multiline.

_ = if True
      then "Yes"
      else "No"
-- `"Yes"`

-- `if` expressions can be multiline without indentation.

_ = if True
    then "Yes"
    else "No"
-- `"Yes"`

-- If the conditional is not a boolean, an error will occur.

{-
_ = if "A" then "Yes" else "No"
-}

-- If the expression in the `then` branch has a different type than the
-- expression in the `else` branch, an error will occur.

{-
_ = if True then "Yes" else 0
-}

-- -----------------------------------------------------------------------------

myAbs :: Integer -> Integer
myAbs x = if x < 0 then -x else x

-- Guard syntax allows functions to behave differently depending on its input.

myAbs :: Integer -> Integer
myAbs x
  | x < 0     = -x
  | otherwise = x

_ = myAbs (-9)    -- `9`
_ = myAbs 9       -- `9`

-- The guard case expression between the pipe character and the equals sign must
-- evaluate to a boolean value.

-- If a guard case evaluates to `True` the function will return the expression
-- the guard was associated to.  If not, the next guard will be considered.
-- Guards always evaluate sequentially, so they should be ordered from the
-- most restrictive case to the least restrictive case.

-- -----------------------------------------------------------------------------

-- A function can have as many clauses as it needs.

dogYears :: Integer -> Integer
dogYears x
  | x <= 0    = 0
  | x <= 1    = x * 15
  | x <= 2    = x * 12
  | x <= 4    = x * 8
  | otherwise = x * 6

_ = dogYears 0    -- `0`
_ = dogYears 1    -- `15`
_ = dogYears 2    -- `24`
_ = dogYears 3    -- `24`
_ = dogYears 4    -- `32`
_ = dogYears 5    -- `30`

-- -----------------------------------------------------------------------------

-- Multiline expressions visually separate guard cases.

dogYears' :: Integer -> Integer
dogYears' x
  | x <= 0    = 0
  | x <= 1    = x * 15
  | x < 2     = x * 12
  | x <= 3    = if x < 3
                then x * 12
                else x * 8
  | x <= 4    = x * 8
  | otherwise = x * 6

_ = dogYears' 0   -- `0`
_ = dogYears' 1   -- `15`
_ = dogYears' 2   -- `24`
_ = dogYears' 3   -- `24`
_ = dogYears' 4   -- `32`
_ = dogYears' 5   -- `30`

-- -----------------------------------------------------------------------------

-- If less restrictive cases appear before more restrictive ones then they will
-- prevent the less restrictive cases from being accessed.

dogYears :: Integer -> Integer
dogYears x
  | x <= 4    = x * 8
  | x <= 0    = 0
  | x <= 1    = x * 15
  | x <= 2    = x * 12
  | otherwise = x * 6

_ = dogYears 0    -- `0`
_ = dogYears 1    -- `8`
_ = dogYears 2    -- `16`
_ = dogYears 3    -- `24`
_ = dogYears 4    -- `32`
_ = dogYears 5    -- `30`

-- -----------------------------------------------------------------------------

-- Guards can be placed next to the function that uses them.

myAbs :: Integer -> Integer
myAbs x | x < 0     = -x
        | otherwise = x

-- Guards don't have to vertically align.

myAbs' :: Integer -> Integer
myAbs' x | x < 0 = -x
  | otherwise = x

_ = myAbs (-9)    -- `9`
_ = myAbs' (-9)   -- `9`
_ = myAbs 9       -- `9`
_ = myAbs' 9      -- `9`

-- -----------------------------------------------------------------------------

-- The `otherwise` keyword represents the catch-all case in our guards and is
-- basically syntactic sugar for `True`.

myAbs :: Integer -> Integer
myAbs x
  | x < 0 = -x
  | True  = x

_ = myAbs (-9)    -- `9`
_ = myAbs 9       -- `9`

-- -----------------------------------------------------------------------------

-- Leading with the catch-all case prevents specific cases from being handled.
-- Unlike with pattern matching, doing this with guards does not incur a
-- compiler warning.

h1 :: Integer -> Integer
h1 x
  | otherwise = x
  | x < 0     = -x

h2 :: Integer -> Integer
h2 x | otherwise = x

h3 :: Integer -> Integer
h3 x = x

_ = h1 (-9)   -- `-9`
_ = h1 (-9)   -- `-9`
_ = h2 (-9)   -- `-9`
_ = h2 9      -- `9`
_ = h3 9      -- `9`
_ = h3 9      -- `9`

-- -----------------------------------------------------------------------------

-- `otherwise` is not always necessary to guard all possible cases.

myAbs :: Integer -> Integer
myAbs x
  | x < 0  = -x
  | x == 0 = x
  | x > 0  = x

_ = myAbs (-9)    -- `9`
_ = myAbs 0       -- `0`
_ = myAbs 9       -- `9`

-- -----------------------------------------------------------------------------

-- If all possible cases are not guarded, we have a partial function.

myAbs :: Integer -> Integer
myAbs x
  | x < 0  = -x
  | x == 0 = x

_ = myAbs (-9)    -- `9`
_ = myAbs 0       -- `0`
_ = myAbs 9       -- Results in a thrown exception.

-- -----------------------------------------------------------------------------

-- Attemping to use the same catch-all case syntax from pattern matching will
-- cause an error.

{-
myAbs :: Integer -> Integer
myAbs x
  | x < 0 = -x
  | _     = x
-}

-- -----------------------------------------------------------------------------

-- Guards can have `where` clauses.

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100
