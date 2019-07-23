module O23__Functor () where

-- A functor is a way to apply a function over or around some structure that we
-- don't want to alter. That is, we want to apply the function to the value
-- that is "inside" some structure and leave the structure alone.

-- `Functor` is a type class for function application "over", or "through",
-- some structure `f` that we want to ignore and leave untouched.

_ = map (\x -> x > 3) [1..6]     -- `[False, False, False, True, True, True]`
_ = fmap (\x -> x > 3) [1..6]    -- `[False, False, False, True, True, True]`

{-
_ = map (+ 1) (Just 1)    -- Error.
-}

_ = fmap (+ 1) (Just 1)                -- `Just 2`
_ = fmap (10 /) (4, 5)                 -- `(4, 2.0)`
_ = fmap (++ " there") (Right "Hi")    -- `Right "Hi there"`

-- `<$>` is the infix alias for `fmap`.

_ = (\x -> x > 3) <$> [1..6]          -- `[False, False, False, True, True, True]`
_ = (+ 1) <$> (Just 1)                -- `Just 2`
_ = (10 /) <$> (4, 5)                 -- `(4, 2.0)`
_ = (++ " there") <$> (Right "Hi")    -- `Right "Hi there"`

-- -----------------------------------------------------------------------------

-- All instances of `Functor` must abide by the functor laws.

-- Identity:
-- fmap id == id

_ = fmap id "Hi Julie"    -- `"Hi Julie"`
_ = id "Hi Julie"         -- `"Hi Julie"`

-- Composition:
-- fmap (f . g) == fmap f . fmap g

_ = fmap ((+ 1) . (* 2)) [1..5]         -- `[3, 5, 7, 9, 11]`
_ = fmap (+ 1) . fmap (* 2) $ [1..5]    -- `[3, 5, 7, 9, 11]`

-- -----------------------------------------------------------------------------

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)

-- A law-abiding instance of `Functor`.
instance Functor WhoCares where
  fmap _ ItDoesnt         = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a)       = Matter (f a)

-- -----------------------------------------------------------------------------

data CountingBad a = Heisenberg Int a deriving (Eq, Show)

-- A law-breaking instance of `Functor`.
instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)

oneWhoKnocks = Heisenberg 0 "Uncle"
f = (<> " Jesse")
g = (<> " lol")

_ = fmap f oneWhoKnocks               -- `Heisenberg 1 "Uncle Jesse"`
_ = fmap g oneWhoKnocks               -- `Heisenberg 1 "Uncle lol"`
_ = fmap (f . g) oneWhoKnocks         -- `Heisenberg 1 "Uncle lol Jesse"`
_ = fmap f . fmap g $ oneWhoKnocks    -- `Heisenberg 2 "Uncle lol Jesse"`

-- The functor composition law was broken because the mapping of `f` caused
-- changes to data that is unrelated to what `f` is actually applied to.

-- -----------------------------------------------------------------------------

data CountingBad' a = Heisenberg' Int a deriving (Eq, Show)

-- A law-abiding instance of `Functor`.
instance Functor CountingBad' where
  fmap f (Heisenberg' n a) = Heisenberg' n (f a)

oneWhoKnocks' = Heisenberg' 0 "Uncle"
f' = (<> " Jesse")
g' = (<> " lol")

_ = fmap f' oneWhoKnocks'                -- `Heisenberg' 0 "Uncle Jesse"`
_ = fmap g' oneWhoKnocks'                -- `Heisenberg' 0 "Uncle lol"`
_ = fmap (f' . g') oneWhoKnocks'         -- `Heisenberg' 0 "Uncle lol Jesse"`
_ = fmap f' . fmap g' $ oneWhoKnocks'    -- `Heisenberg' 0 "Uncle lol Jesse"`

-- The functor composition law was followed because the mapping of `f` caused
-- changes to only what `f` is actually applied to.

-- -----------------------------------------------------------------------------

-- Functions have an instance of `Functor` with their mapping as composition.

_ = fmap (+ 1) negate $ 5    -- `-4`
_ = (+ 1) <$> negate $ 5     -- `-4`
_ = (+ 1) . negate $ 5       -- `-4`

-- -----------------------------------------------------------------------------

-- Nested functors can be mapped at different "depths".

p = const 'p'

x = [Just "Ave", Nothing, Just "woohoo"]

_ = p x                         -- `p`
_ = fmap p x                    -- `"ppp"`
_ = (fmap . fmap) p x           -- `[Just 'p', Nothing, Just 'p']`
_ = (fmap . fmap . fmap) p x    -- `[Just "ppp", Nothing, Just "pppppp"]`

y = [Just ["Ha", "Ha"], Nothing, Just []]

_ = p y                                -- `p`
_ = fmap p y                           -- `"ppp"`
_ = (fmap . fmap) p y                  -- `[Just 'p', Nothing, Just 'p']`
_ = (fmap . fmap . fmap) p y           -- `[Just "pp", Nothing, Just ""]`
_ = (fmap . fmap . fmap . fmap) p y    -- `[Just ["pp", "pp"], Nothing, Just []]`

-- -----------------------------------------------------------------------------

-- But what if you do want a function that can change the value and the
-- structure? Just use a normal function to do that.

-- The point of Functor is to reify and be able to talk about cases where we
-- want to reuse functions in the presence of more structure and be
-- transparently oblivious to that additional structure.












_ = (+1) <$> read "[1]" :: [Int]    -- `[2]`

_ = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])    -- `Just ["Hi,lol", "Hellolol"]`




-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

Functor: A way to apply a function over a structure that we don't want to alter.
Lift: To preserve a structure while applying a function over its values.

-}
