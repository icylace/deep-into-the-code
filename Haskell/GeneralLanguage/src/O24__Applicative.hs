module O24__Applicative () where

-- Applicatives are monoidal functors.

-- `Monoid` gives us a means of mashing two values of the same type together.
-- `Monoid`'s core operation, `mappend`, smashes the structures together -
-- when you mappend two lists, they become one list, so the structures
-- themselves have been joined.

-- `Functor` is for function application over some structure we don't want to
-- have to think about. The core operation of `Functor`, `fmap`, applies a
-- function to a value that is within some structure while leaving that
-- structure unaltered.

-- `Applicative` allows for function application lifted over structure (like
-- Functor). But with `Applicative` the function we're applying is also
-- embedded in some structure. Because the function and the value it's
-- being applied to both have structure, we have to smash those
-- structures together. So, `Applicative` involves monoids
-- and functors.

-- This is only used for reference.
class Functor f => Applicative' f where
  pure' :: a -> f a
  (<*>.) :: f (a -> b) -> f a -> f b

-- The application operator, `$`, map operator, `<$>`, from `Functor`, and the
-- apply operator (a.k.a. "ap"), `<*>`, from `Applicative` are very similar.
--
--  ($)  ::                    (a -> b) ->   a ->   b
-- (<$>) ::     Functor f =>   (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- A functor can be defined in terms of an applicative.
--
-- fmap f x = pure f <*> x

_ = fmap (+1) [1, 2, 3]     -- `[2, 3, 4]`
_ = pure (+1) <*> [1..3]    -- `[2, 3, 4]`

-- From the `Control.Applicative` module:
--
-- liftA  :: Applicative f => (a -> b)           -> f a -> f b
-- liftA2 :: Applicative f => (a -> b -> c)      -> f a -> f b -> f c
-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

_ = pure 1 :: [Int]           -- `[1]`
_ = pure 1 :: Maybe Int       -- `Just 1`
_ = pure 1 :: Either a Int    -- `Right 1`
_ = pure 1 :: ([a], Int)      -- `([], 1)`

-- The left type is handled differently from the right in the final two examples
-- for the same reason as here:

_ = fmap (+1) (4, 5)    -- `(4, 6)`

-- The left type is part of the structure, and the structure is not transformed
-- by the function application.

-- With `Applicative` we have a `Monoid` for our structure and function
-- application for our values.
--
-- mappend :: f          -> f   -> f
-- $       ::   (a -> b) ->   a ->   b
-- (<*>)   :: f (a -> b) -> f a -> f b

_ = [(*2), (*3)] <*> [4, 5]    -- `[8, 10, 12, 15]`
-- `[2 * 4, 2 * 5, 3 * 4, 3 * 5]`
-- `[8, 10, 12, 15]`

_ = Just (*2) <*> Just 2     -- `Just 4`
_ = Just (*2) <*> Nothing    -- `Nothing`
_ = Nothing <*> Just 2       -- `Nothing`
_ = Nothing <*> Nothing      -- `Nothing`
