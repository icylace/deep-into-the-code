module O14__Equality where

x :: Integer
x = 9

-- The equality operator, `==`, checks if a couple expressions are equal.

_ = x == 5    -- `False`
_ = x == 9    -- `True`
_ = x == 10   -- `False`

_ = 'a' == 'a'    -- `True`
_ = 'a' == 'b'    -- `False`
_ = 'a' == 'A'    -- `False`

_ = "Hello" == "Hello"    -- `True`
_ = "Hello" == "hello"    -- `False`
_ = "Hello" == "World"    -- `False`

-- The inequality operator, `/=`, checks if a couple expressions are not equal.

_ = x /= 5    -- `True`
_ = x /= 9    -- `False`
_ = x /= 10   -- `True`

-- Attempting to compare values having different types will cause an error.
--
--     'a' == 8
--

-- The `Eq` typeclass defines equality comparison.

-- Datatypes must have an instance of the `Eq` typeclass for comparison to work.

data Me1 = Me1

instance Eq Me1 where
  Me1 == Me1 = True

_ = Me1 == Me1    -- `True`

-- When only equality is defined, inequality is automatically derived.

_ = Me1 /= Me1    -- `False`

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

data Me4 = Me4

instance Eq Me4 where
  (==) Me4 Me4 = False
  (/=) Me4 Me4 = False

_ = Me4 == Me4    -- `False`
_ = Me4 /= Me4    -- `False`

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

-- More `Eq` examples.

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y

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
  (==) (TisAnInt x)   (TisAnInt y)   = x == y
  (==) (TisAString x) (TisAString y) = x == y
  (==) _              _              = False

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

_ = Tuple 1 2   == Tuple 1 2        -- `True`
_ = Tuple 1 'a' == Tuple 1 'a'    -- `True`
_ = Tuple 'b' 3 == Tuple 'c' 3    -- `False`

-- -----------------------------------------------------------------------------

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) _ _ = False

_ = ThisOne 'a' == ThisOne 'a'    -- `True`
_ = ThisOne 'a' == ThisOne 'b'    -- `False`
_ = ThatOne 2   == ThisOne 2      -- `False`
_ = ThatOne 'g' == ThatOne 'g'    -- `True`

-- -----------------------------------------------------------------------------

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x)   (Hello y)   = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _           _           = False

a :: EitherOr Integer Char
a = Hello 1

b :: EitherOr Integer Char
b = Goodbye 'v'

_ = a == a    -- `True`
_ = a == b    -- `False`
_ = b == a    -- `False`
_ = b == b    -- `True`













-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
