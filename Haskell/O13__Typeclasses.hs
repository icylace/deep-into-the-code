module O13__Typeclasses where

  -- New datatypes can be created using the `data` keyword.

data Me = Me

-- The type constructor and data constructor don't have to have the same name.

data Self = Myself

-- Equality comparison with our custom `Me` and `Self` types won't work.
--
--     Me == Me
--     Myself == Myself
--





-- Typeclass instances are unique to a given type.  Attempting to create
-- multiple instances of the same typeclass for the same type will
-- result in an error.












-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Certain typeclasses can have instances of them "derived".  A derived instance
-- is an automatically generated instance.

data I = I deriving Eq

_ = I == I    -- `True`
_ = I /= I    -- `False`

-- The common typeclasses that can be derived are `Eq`, `Ord`, `Enum`,
-- `Bounded`, `Read`, and `Show`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Datatypes must have an instance of the `Show` typeclass to be printable.

data You = You deriving Show

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------





-- Polymorphic types can use constraints in their instance declarations
-- to require their concrete types to have instances as well.

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-- Polymorphic types can use constraints in their instance declarations
-- to require their concrete types to have instances as well.

-- -----------------------------------------------------------------------------

-- It's possible to use a looser constraint to achieve the same thing.
-- In this case, the `Ord` typeclass can be used because it already
-- requires the use of `Eq`.

data Identity' a = Identity' a

instance Ord a => Eq (Identity' a) where
  (==) (Identity' v) (Identity' v') = compare v v' == EQ

-- However, doing it this way would be unnecessarily less precise.

-- -----------------------------------------------------------------------------

-- Loosened constraints can be used in normal type signatures, too.

check' :: Eq a => a -> a -> Bool
check' x y = x == y

check'' :: Ord a => a -> a -> Bool
check'' x y = x == y







-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Some standard Haskell typeclasses have a default concrete type which gets
-- used if the concrete type can't be determined when needed.

-- The division operator, `/`, is part of the `Fractional` typeclass and has
-- the type `Fractional a => a -> a -> a`.  `Fractional` defaults to the
-- concrete type `Double`.  An explicit type declaration can be used
-- to override the default.

_ = 1 / 2                         -- `0.5`
_ = 1 / 2 :: Fractional a => a    -- `0.5`
_ = 1 / 2 :: Float                -- `0.5`
_ = 1 / 2 :: Double               -- `0.5`
_ = 1 / 2 :: Rational             -- `1 % 2`

-- We get an error if we try to use explicit type signatures to more generalize
-- an expression.
--
-- For example, even though `Fractional` is a subclass of `Num`, an expression
-- having a `Fractional` type cannot be generalized into having a `Num` type.
--
--     1 / 2 :: Num a => a
--


-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------





-- Values having concrete type will necessarily concretize polymorphic
-- functions that are applied to them.

-- The addition operator, `+`, from the `Num` typeclass has this type:
--     (+) :: Num a => a -> a -> a

x = 10 :: Integer
y = 5  :: Integer

_ = (+)       -- Has the type `Num a => a -> a -> a`.
_ = (x +)     -- Has the type `Integer -> Integer`.
_ = (x + y)   -- Has the type `Integer`.









-- Typeclass inheritance is only additive.  That is, a subclass cannot override
-- a superclass in any way.










-- A typeclass instance should be kept in the same file as the type that has it.





-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
