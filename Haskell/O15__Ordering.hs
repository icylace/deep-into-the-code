module O15__Ordering where

-- The `Ord` typeclass defines order comparison.

x :: Integer
x = 9

-- The less-than operator, `<`, checks if an expression is lesser than another.

_ = x < 5     -- `False`
_ = x < 9     -- `False`
_ = x < 10    -- `True`

_ = 'a' < 'b'     -- `True`
_ = "Hello" < "hello"     -- `True`
_ = "Hello" < "World"     -- `True`

-- The less-than-or-equal-to operator, `<=`, checks if an expression is
-- lesser than or equal to another.

_ = x <= 5    -- `False`
_ = x <= 9    -- `True`
_ = x <= 10   -- `True`

-- -----------------------------------------------------------------------------

-- The greater-than operator, `>`, checks if an expression is greater
-- than another.

_ = x > 5     -- `True`
_ = x > 9     -- `False`
_ = x > 10    -- `False`

_ = 'a' > 'b'     -- `False`
_ = "Hello" > "hello"     -- `False`
_ = "Hello" > "World"     -- `False`

-- The greater-than-or-equal-to operator, `>=`, checks if an expression is
-- greater than or equal to another.

_ = x >= 5    -- `True`
_ = x >= 9    -- `True`
_ = x >= 10   -- `False`

-- -----------------------------------------------------------------------------

-- The `compare` function describes the ordering between two values.

_ = compare 7 8               -- `LT`
_ = compare 4 (-4)            -- `GT`
_ = compare 4 4               -- `EQ`
_ = compare "Julie" "Chris"   -- `GT`
_ = compare True False        -- `GT`
_ = compare True True         -- `EQ`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

_ = max 7 8               -- `8`
_ = max (3, 4) (2, 3)     -- `(3,4)`
_ = max "Julie" "Chris"   -- `"Julie"`
_ = max 7 (max 8 9)       -- `9`

_ = min 10 (-10)                    -- `-10`
_ = min [2, 3, 4, 5] [3, 4, 5, 6]   -- `[2,3,4,5]`
_ = min 'f' 'd'                     -- `d`
_ = min 7 (min 8 9)                 -- `7`

-- -----------------------------------------------------------------------------

-- Deriving an instance for the `Ord` typeclass relies on the way the datatype
-- is defined to determine ordering.

data Suit = Spades | Hearts | Clubs | Diamonds deriving (Eq, Ord)

-- Since `Ord` is dependent on `Eq`, trying to go without it is an error.

{-
data Suit = Spades | Hearts | Clubs | Diamonds deriving (Ord)
-}

-- Values to the left are considered to be less than values to the right.

_ = Hearts < Spades     -- `False`
_ = Hearts < Hearts     -- `False`
_ = Hearts < Clubs      -- `True`
_ = Hearts < Diamonds   -- `True`

-- -----------------------------------------------------------------------------

-- An `Ord` instance can be manually created, of course.

data Suit' = Spades' | Hearts' | Clubs' | Diamonds' deriving (Eq)

instance Ord Suit' where
  compare Hearts' Hearts' = EQ
  compare Hearts' _       = GT
  compare _       Hearts' = LT
  compare _       _       = EQ

_ = Hearts' < Spades'     -- `False`
_ = Hearts' < Hearts'     -- `False`
_ = Hearts' < Clubs'      -- `False`
_ = Hearts' < Diamonds'   -- `False`

-- -----------------------------------------------------------------------------

-- data Ordering = LT | EQ | GT

_ = compare 10 9    -- `GT`
_ = compare 9 9     -- `EQ`
_ = compare 9 10    -- `LT`
