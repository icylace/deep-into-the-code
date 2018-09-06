module O12__Lists where




-- A range of values can be created with some shorthand notation.

_ = [0..5]        -- `[0, 1, 2, 3, 4, 5]`
_ = ['a'..'z']    -- `"abcdefghijklmnopqrstuvwxyz"`








-- Lists of the same type can be concatenated together.

_ = ['a'] ++ ['b']    -- `"ab"`
_ = ['a'] ++ "b"      -- `"ab"`
_ = "a" ++ ['b']      -- `"ab"`
_ = "a" ++ "b"        -- `"ab"`
_ = ["a"] ++ ["b"]    -- `["a","b"]`
_ = [1] ++ [2]        -- `[1, 2]`

-- Attempting to concatenate lists of different types will cause an error.

{-
_ = ['a'] ++ [2]
_ = "a" ++ [2]
_ = [1] ++ ['b']
_ = [1] ++ "b"
-}

-- The append operator, `++`, is an example of a polymorphic function meaning it
-- can work with different types as long as those types are used with
-- it correctly.






-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
