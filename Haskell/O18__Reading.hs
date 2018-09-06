module O18__Reading where

-- The `Read` typeclass defines how strings can be parsed into data.

-- `read` attempts to return data from a parsed string and itt has the type
-- `Read a => String -> a`.

_ = read "1234567" :: Integer     -- `1234567`
_ = (read "1234567") :: Integer   -- `1234567`

-- Not all string values can be successfully converted to the desired type.

_ = read "BOOM" :: Integer    -- Results in an exception.

-- Therefore, `read` is a partial function.
