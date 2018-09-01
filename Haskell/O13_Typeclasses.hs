module O13_Typeclasses where



-- New datatypes can be created using the `data` keyword.

data Me = Me

-- The type constructor and data constructor don't have to have the same name.

data Self = Myself

-- Equality comparison with our custom `Me` and `Self` types won't work.
--
--     Me == Me
--     Myself == Myself
--

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Datatypes must have an instance of the `Eq` typeclass for comparison to work.

data Me' = Me'

instance Eq Me' where
  Me' == Me' = True

_ = Me' == Me'    -- `True`

-- When only equality is defined, inequality is automatically derived.

_ = Me' /= Me'    -- `False`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

data Me2 = Me2

instance Eq Me2 where
  Me2 /= Me2 = True

_ = Me2 /= Me2    -- `True`

-- When only inequality is defined, equality is automatically derived.

_ = Me2 == Me2    -- `False`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

data Me3 = Me3

-- Both equality and inequality can be manually defined.

instance Eq Me3 where
  Me3 == Me3 = True
  Me3 /= Me3 = True

_ = Me3 == Me3    -- `True`
_ = Me3 /= Me3    -- `True`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

data Me4 = Me4

instance Eq Me4 where
  (==) Me4 Me4 = False
  (/=) Me4 Me4 = False

_ = Me4 == Me4    -- `False`
_ = Me4 /= Me4    -- `False`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Typeclass instances are unique to a given type.  Attempting to create
-- multiple instances of the same typeclass for the same type will
-- result in an error.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

data Weekday = Monday | Tuesday | Wednesday | Thursday
             | Friday | Saturday | Sunday

data Date = Date Weekday Int

instance Eq Weekday where
  (==) Monday    Monday    = True
  (==) Tuesday   Tuesday   = True
  (==) Wednesday Wednesday = True
  (==) Thursday  Thursday  = True
  (==) Friday    Friday    = True
  (==) Saturday  Saturday  = True
  (==) Sunday    Sunday    = True
  (==) _         _         = False

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'

_ = Date Thursday 10 == Date Thursday 10    -- `True`
_ = Date Thursday 10 == Date Thursday 11    -- `False`
_ = Date Thursday 10 == Date Wednesday 10   -- `False`

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
-- -----------------------------------------------------------------------------

-- It's possible to use a looser constraint to achieve the same thing.
-- In this case, the `Ord` typeclass can be used because it already
-- requires the use of `Eq`.

data Identity' a = Identity' a

instance Ord a => Eq (Identity' a) where
  (==) (Identity' v) (Identity' v') = compare v v' == EQ

-- However, doing it this way would be unnecessarily less precise.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- More `Eq` examples.

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn x') = x == x'

_ = TisAn 4 == TisAn 4    -- `True`
_ = TisAn 3 == TisAn 2    -- `False`

-- -----------------------------------------------------------------------------

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x x') (Two y y') = x == x' && y == y'

_ = Two 1 1 == Two 1 1    -- `True`
_ = Two 1 3 == Two 1 4    -- `False`

-- -----------------------------------------------------------------------------

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt x') = x == x'
  (==) (TisAString x) (TisAString x') = x == x'
  (==) _ _ = False

_ = TisAnInt 3     == TisAnInt 3        -- `True`
_ = TisAnInt 2     == TisAnInt 4        -- `False`
_ = TisAString ""  == TisAString ""     -- `True`
_ = TisAString "a" == TisAString "b"    -- `False`
_ = TisAnInt 2     == TisAString "b"    -- `False`
_ = TisAString "a" == TisAnInt 2        -- `False`

-- -----------------------------------------------------------------------------

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

_ = Pair 0 0     == Pair 0 0        -- `True`
_ = Pair 2 3     == Pair 2 3        -- `True`
_ = Pair 1 3     == Pair 2 1        -- `False`
_ = Pair 'a' 'v' == Pair 'a' 'v'    -- `True`
_ = Pair 'b' 'c' == Pair 'c' 'b'    -- `False`

-- -----------------------------------------------------------------------------

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

_ = Tuple 1 2 == Tuple 1 2        -- `True`
_ = Tuple 1 'a' == Tuple 1 'a'    -- `True`
_ = Tuple 'b' 3 == Tuple 'c' 3    -- `False`

-- -----------------------------------------------------------------------------

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _ = False

_ = ThisOne 'a' == ThisOne 'a'    -- `True`
_ = ThisOne 'a' == ThisOne 'b'    -- `False`
_ = ThatOne 2   == ThisOne 2      -- `False`
_ = ThatOne 'g' == ThatOne 'g'    -- `True`

-- -----------------------------------------------------------------------------

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye x) (Goodbye x') = x == x'
  (==) _ _ = False

a :: EitherOr Integer Char
a = Hello 1

b :: EitherOr Integer Char
b = Goodbye 'v'

_ = a == a    -- `True`
_ = a == b    -- `False`
_ = b == a    -- `False`
_ = b == b    -- `True`

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
y = 5 :: Integer

_ = (+)       -- Has the type `Num a => a -> a -> a`.
_ = (x +)     -- Has the type `Integer -> Integer`.
_ = (x + y)   -- Has the type `Integer`.









-- Typeclass inheritance is only additive.  That is, a subclass cannot override
-- a superclass in any way.










-- A typeclass instance should be kept in the same file as the type that has it.




-- Some common typeclasses:
-- - `Eq` provides instances for determining equality of values.





-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
