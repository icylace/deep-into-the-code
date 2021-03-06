module O16__Enumeration () where

-- The `Enum` type class defines enumeration.

-- Enumerable values have known successors and predecessors.

_ = succ 4        -- `5`
_ = pred 4        -- `3`

_ = succ 4.5      -- 5.5
_ = pred 4.5      -- 3.5

_ = succ 'd'      -- `e`
_ = pred 'd'      -- `c`

_ = succ False    -- `True`
_ = pred True     -- `False`

-- -----------------------------------------------------------------------------

-- An exception will be thrown if a successor predecessor doesn't exist.

_ = succ True               -- Exception.
_ = pred False              -- Exception.

_ = pred minBound :: Int    -- Exception.
_ = pred maxBound :: Int    -- `9223372036854775806`

_ = succ minBound :: Int    -- `-9223372036854775807`
_ = succ maxBound :: Int    -- Exception.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- `enumFromTo` creates a list of sequential values within a range. It behaves
-- the same way as the range syntax, `..`.

_ = enumFromTo 0 0            -- `[0]`
_ = [0..0]                    -- `[0]`

_ = enumFromTo 3 8            -- `[3, 4, 5, 6, 7, 8]`
_ = [3..8]                    -- `[3, 4, 5, 6, 7, 8]`

_ = enumFromTo 'a' 'a'        -- `"a"`
_ = ['a'..'a']                -- `"a"`

_ = enumFromTo 'a' 'f'        -- `"abcdef"`
_ = ['a'..'f']                -- `"abcdef"`

_ = enumFromTo False False    -- `[False]`
_ = enumFromTo False True     -- `[False, True]`
_ = enumFromTo True  False    -- `[]`
_ = enumFromTo True  True     -- `[True]`

-- -----------------------------------------------------------------------------

-- `enumFromThenTo` creates a list of values within a range by first noting the
-- gap between it's first couple arguments then using that gap to fill in the
-- rest of the range.

_ = enumFromThenTo 1 10 100       -- `[1, 10, 19, 28, 37, 46, 55, 64, 73, 82, 91, 100]`
_ = enumFromThenTo 0 10 100       -- `[0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]`
_ = enumFromThenTo 0 10 77        -- `[0, 10, 20, 30, 40, 50, 60, 70]`

_ = enumFromThenTo 0 0 0          -- Infinite list of `0`.
_ = enumFromThenTo 0 0 1          -- Infinite list of `0`.
_ = enumFromThenTo 0 1 0          -- `[0]`
_ = enumFromThenTo 0 1 1          -- `[0, 1]`
_ = enumFromThenTo 1 0 0          -- `[1, 0]`
_ = enumFromThenTo 1 0 1          -- `[1]`
_ = enumFromThenTo 1 1 0          -- `[]`
_ = enumFromThenTo 1 1 1          -- Infinite list of `1`.

_ = enumFromThenTo 'a' 'a' 'a'    -- Infinite list of `a`.
_ = enumFromThenTo 'a' 'c' 'z'    -- `"acegikmoqsuwy"`
_ = enumFromThenTo 'b' 'a' 'f'    -- `""`

_ = enumFromThenTo False False False    -- Infinite list of `False`.
_ = enumFromThenTo False False True     -- Infinite list of `False`.
_ = enumFromThenTo False True  False    -- `[False]`
_ = enumFromThenTo False True  True     -- `[False, True]`
_ = enumFromThenTo True  False False    -- `[True, False]`
_ = enumFromThenTo True  False True     -- `[True]`
_ = enumFromThenTo True  True  False    -- `[]`
_ = enumFromThenTo True  True  True     -- Infinite list of `True`.
