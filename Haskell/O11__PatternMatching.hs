module O11__PatternMatching () where

-- Pattern matching is a way of matching values against patterns and, where
-- appropriate, binding variables to successful matches.

-- Patterns are matched against values, or data constructors, not types.
-- Matching a pattern may fail, procedding to the next available pattern
-- to match or succeed.  When a match succeeds, the variables exposed in
-- the pattern are bound.  Pattern matching proceeds from left to right
-- and outside to inside.

-- The underscore here represents a "catch-all" case that never fails to match.

f :: Integer -> Bool
f 2 = True
f _ = False

_ = f 1     -- `False`
_ = f 2     -- `True`
_ = f 3     -- `False`
_ = f 50    -- `False`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The first pattern that is matched against will be used.

f' :: Integer -> Bool
f' _ = False
f' 2 = True

_ = f' 1     -- `False`
_ = f' 2     -- `False`
_ = f' 3     -- `False`
_ = f' 50    -- `False`

-- The definition of `f'` will incur a warning because its catch-all case is
-- matched first which prevents every other case coming after it from
-- ever matching.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- A function will return "bottom", a non-value, when it's applied to an
-- appropriately typed argument it doesn't know how to handle.

-- Evaluating a function applied to an unhandled argument throws an exception.

-- Functions of this sort are called partial functions.

-- A partial function can implicitly not handle all cases.

f'' :: Integer -> Bool
f'' 2 = True

_ = f'' 1     -- Results in a thrown exception.
_ = f'' 2     -- `True`
_ = f'' 3     -- Results in a thrown exception.
_ = f'' 50    -- Results in a thrown exception.

-- -----------------------------------------------------------------------------

-- A partial function can also explicitly not handle all cases.

-- The special `undefined` value is a way to explicitly use bottom.

g :: Integer -> Bool
g 2 = True
g _ = undefined

_ = g 1     -- Results in a thrown exception.
_ = g 2     -- `True`
_ = g 3     -- Results in a thrown exception.
_ = g 50    -- Results in a thrown exception.

-- ------------------------------------------------------------------------------- -----------------------------------------------------------------------------


fst' :: (a, b) -> a
fst' (a, _) = a

snd' :: (a, b) -> b
snd' (_, b) = b

tup :: (Integer, [a]) -> (Integer, [a]) -> (Integer, [a])
tup (a, b) (c, d) = ((a + c), (b ++ d))




-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Pattern matching: Identifying arguments that follow a given pattern.
-- Partial function: A function that does not handle all possible inputs.
-- Total function: A function that does handle all possible inputs.
