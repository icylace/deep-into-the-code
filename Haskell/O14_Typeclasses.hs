module O14_Typeclasses where



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

_ = Me' == Me'
-- Result will be `True`.

-- When only equality is defined, inequality is automatically derived.

_ = Me' /= Me'
-- Result will be `False`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

data Me2 = Me2

instance Eq Me2 where
  Me2 /= Me2 = True

_ = Me2 /= Me2
-- Result will be `True`.

-- When only inequality is defined, equality is automatically derived.

_ = Me2 == Me2
-- Result will be `False`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

data Me3 = Me3

-- Both equality and inequality can be manually defined.

instance Eq Me3 where
  Me3 == Me3 = True
  Me3 /= Me3 = True

_ = Me3 == Me3
_ = Me3 /= Me3
-- Both results will be `True`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

data Me4 = Me4

instance Eq Me4 where
  (==) Me4 Me4 = False
  (/=) Me4 Me4 = False

_ = Me4 == Me4
_ = Me4 /= Me4
-- Both results will be `False`.

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

_ = Date Thursday 10 == Date Thursday 10
-- Result will be `True`.

_ = Date Thursday 10 == Date Thursday 11
-- Result will be `False`.

_ = Date Thursday 10 == Date Wednesday 10
-- Result will be `False`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Certain typeclasses can have instances of them "derived".  A derived instance
-- is an automatically generated instance.

data I = I deriving Eq

_ = I == I
-- Result will be `True`.

_ = I /= I
-- Result will be `False`.

-- The common typeclasses that can be derived are `Eq`, `Ord`, `Enum`,
-- `Bounded`, `Read`, and `Show`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Datatypes must have an instance of the `Show` typeclass to be printable.

data You = You deriving Show

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------






-- A typeclass instance should be kept in the same file as the type that has it.




-- Some common typeclasses:
-- - `Eq` provides instances for determining equality of values.





-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
