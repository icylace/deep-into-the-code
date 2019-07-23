module O22__SemigroupAndMonoid () where

import Data.Monoid
  ( Any(..)
  , All(..)
  , First(..)
  , Last(..)
  , Product(..)
  , Sum(..)
  )

-- The `Monoid` type class generalizes associativity and identity across types.

-- `mappend` acts like `++`.

_ = (++) [1, 2, 3] [4, 5, 6]       -- `[1, 2, 3, 4, 5, 6]`
_ = mappend [1, 2, 3] [4, 5, 6]    -- `[1, 2, 3, 4, 5, 6]`

-- The empty list `[]` is the identity of lists.

_ = mappend [1..5] []    -- `[1, 2, 3, 4, 5]`
_ = mappend [] [1..5]    -- `[1, 2, 3, 4, 5]`

-- -----------------------------------------------------------------------------

-- None of the numeric types have instances of `Monoid` due to ambiguity of
-- whether to make addition or multiplication the "default" monoid.

-- Newtypes can be used to enforce the unique instance rule by separating
-- any different monoidal behaviors a type may have.

-- The `Data.Monoid` module has the `Sum` and `Product` newtypes which resolve
-- this ambiguity.

_ = mappend (Sum 1) (Sum 5)            -- `Sum {getSum = 6}`
_ = mappend (Product 5) (Product 5)    -- `Product {getProduct = 25}`
_ = mappend (Sum 4.5) (Sum 3.4)        -- `Sum {getSum = 7.9}`

-- -----------------------------------------------------------------------------

-- `mappend` is a binary operation.

_ = mappend (Sum 1) (mappend (Sum 2) (Sum 3))    -- Sum {getSum = 6}

{-
_ = mappend (Sum 8) (Sum 9) (Sum 10)    -- Error.
-}

-- -----------------------------------------------------------------------------

-- `<>` is the operator version of `mappend`.

_ = mappend (Sum 8) (Sum 9)    -- Sum {getSum = 17}
_ = Sum 8 <> Sum 9             -- Sum {getSum = 17}

_ = Sum 1 <> Sum 2 <> Sum 3    -- Sum {getSum = 6}

-- -----------------------------------------------------------------------------

-- `mconcat` is like `mappend` but works on lists.

_ = mconcat [Sum 1, Sum 2, Sum 3]    -- Sum {getSum = 6}

-- -----------------------------------------------------------------------------

-- `mempty` is a generic identity.

_ = mappend mempty Sum 9    -- Sum {getSum = 9}
_ = mempty <> Sum 9         -- Sum {getSum = 9}

-- -----------------------------------------------------------------------------

-- `getSum` accesses the wrapped number within `Sum`.

_ = getSum $ mappend (Sum 1) (Sum 1)    -- `2`

-- `getProduct` accesses the wrapped number within `Product`.

_ = getProduct $ mappend (Product 5) (Product 5)    -- `25`

-- -----------------------------------------------------------------------------

-- `mconcat` is equivalent to `foldr mappend mempty`

_ = foldr mappend mempty ([2, 4, 6] :: [Product Int])    -- `Product {getProduct = 48}`
_ = foldr mappend mempty ([2, 4, 6] :: [Sum Int])        -- `Sum {getSum = 12}`
_ = foldr mappend mempty ["blah", "woot"]                -- `"blahwoot"`

_ = mconcat ([2, 4, 6] :: [Product Int])    -- `Product {getProduct = 48}`
_ = mconcat ([2, 4, 6] :: [Sum Int])        -- `Sum {getSum = 12}`
_ = mconcat ["blah", "woot"]                -- `"blahwoot"`

-- -----------------------------------------------------------------------------

-- All instances of `Monoid` must abide by the monoidal laws:
--
-- Left identity:
-- mappend mempty x == x
--
-- Right identity:
-- mappend x mempty == x
--
-- Associativity:
-- mappend x (mappend y z) == mappend (mappend x y) z

_ = Sum 1                        -- `Sum {getSum = 1}`
_ = mappend mempty (Sum 1)       -- `Sum {getSum = 1}`
_ = mappend (Sum 1) mempty       -- `Sum {getSum = 1}`
_ = Sum 1 <> (Sum 2 <> Sum 3)    -- `Sum {getSum = 6}`
_ = (Sum 1 <> Sum 2) <> Sum 3    -- `Sum {getSum = 6}`

_ = mappend mempty [1, 2, 3]    -- `[1, 2, 3]`
_ = mappend [1, 2, 3] mempty    -- `[1, 2, 3]`
_ = [1] <> ([2] <> [3])         -- `[1, 2, 3]`
_ = ([1] <> [2]) <> [3]         -- `[1, 2, 3]`

-- -----------------------------------------------------------------------------

-- When we have more than one potential implementation for Monoid for a
-- datatype, it's most convenient to use newtypes to tell them apart,
-- as we did with Sum and Product.

-- `All` represents boolean conjunction.

_ = All True <> All True     -- `All {getAll = True}`
_ = All True <> All False    -- `All {getAll = False}`

-- `Any` represents boolean disjunction.

_ = Any True <> Any False     -- `Any {getAny = True}`
_ = Any False <> Any False    -- `Any {getAny = False}`

-- `First` returns the first or leftmost non-`Nothing` value.

_ = First (Just 1) <> First (Just 2)    -- `First {getFirst = Just 1}`
_ = First Nothing <> First (Just 2)     -- `First {getFirst = Just 2}`
_ = First Nothing <> First Nothing      -- `First {getFirst = Nothing}`

-- `Last` returns the last or rightmost non-`Nothing` value.

_ = Last (Just 1) <> Last (Just 2)    -- `Last {getLast = Just 2}`
_ = Last Nothing <> Last (Just 2)     -- `Last {getLast = Just 2}`
_ = Last Nothing <> Last Nothing      -- `Last {getLast = Nothing}`

-- -----------------------------------------------------------------------------

-- For phantom types, a monoid instance for its type argument is not required.

data Booly a = False' | True' deriving (Eq, Show)

-- The monoid for our custom type needs to have a semigroup for the same type.
instance Semigroup (Booly a) where
  (<>) False' _      = False'
  (<>) _      False' = False'
  (<>) True'  True'  = True'

instance Monoid (Booly a) where
  mempty = True'

-- -----------------------------------------------------------------------------

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada     <> x        = x
  x        <> Nada     = x
  (Only x) <> (Only y) = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

_ = Only (Sum 1) <> Only (Sum 1)            -- Only (Sum {getSum = 2})
_ = Only (Product 4) <> Only (Product 2)    -- Only (Product {getProduct = 8})
_ = Only (Sum 1) `mappend` Nada             -- Only (Sum {getSum = 1})
_ = Only [1] `mappend` Nada                 -- Only [1]
_ = Nada `mappend` Only (Sum 1)             -- Only (Sum {getSum = 1})

-- -----------------------------------------------------------------------------

-- Commutativity is a useful property and can be helpful in circumstances when
-- you might need to be able to reorder evaluation of your data for efficiency
-- purposes without needing to worry about the result changing.

-- -----------------------------------------------------------------------------

-- An identity is a value with a special relationship with an operation: it
-- turns the operation into the identity function. There are no identities
-- without operations. The concept is defined in terms of its relationship
-- with a given operation.

-- -----------------------------------------------------------------------------





-- TODO:
-- We do sometimes end up with multiple instances for a single type when orphan
-- instances are written. But writing orphan instances should be avoided at all
-- costs. If you get an orphan instance warning from GHC, fix it.
--
-- An orphan instance is when an instance is defined for a datatype and type
-- class, but not in the same module as either the declaration of the type
-- class or the datatype. If you don't own the type class or the datatype,
-- newtype it!
--
-- If you want an orphan instance so that you can have multiple instances for
-- the same type, you still want to use `newtype`.



-- Orphan instances make it possible for type class functions to behave
-- differently depending on what modules are imported. This makes it
-- more difficult to predict the behavior of type class instances.





-- There are a few solutions for addressing orphan instances:
--
-- 1. You defined the type but not the type class? Put the instance in the same
--    module as the type so that the type cannot be imported without its
--    instances.
-- 2. You defined the type class but not the type? Put the instance in the same
--    module as the type class definition so that the type class cannot be
--    imported without its instances.
-- 3. Neither the type nor the type class are yours? Define your own newtype
--    wrapping the original type and now you've got a type that "belongs" to
--    you for which you can rightly define type class instances.


-- -----------------------------------------------------------------------------

-- https://github.com/larrybotha/haskell-book/blob/master/chapter-15/15.15-semigroup-exercises-11.hs

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Success x <> _         = Success x
  _         <> Success y = Success y
  Failure x <> Failure y = Failure (x <> y)

failure :: String -> Validation String Int
failure = Failure

success :: Int -> Validation String Int
success = Success

tryValidation :: IO ()
tryValidation = do
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2



-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

Algebra: The study of math symbols and the rules governing their manipulation.
Law: A rule about how an algebra or structure should behave.
Semigroup: A set closed under an associative binary operation.
Monoid: A set closed under an associative binary operation with an identity.
Identity: A value, combined with another value, always resulting in the latter.
Associativity: When values can be combined in another grouping for same result.
Commutativity: When values can be combined in another order for the same result.
Orphan instance: An instance defined in a module away from type class/datatype.

-}
