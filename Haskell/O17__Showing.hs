module O17__Showing () where

-- The `Show` typeclass defines how data can be printable.

-- `Show` is only suitable for human readability and not serialization.

-- Serialization is the process of converting data into a format suitable for
-- transmission to some place like a file or database or over a network.

data You = You deriving Show
data Hoo = Hoo deriving (Show)

-- `print` prints a printable value to the console and has the type
-- `Show a => a -> IO ()`.

p1 :: IO ()
p1 = print You

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

data Mood = Blah

instance Show Mood where
  show _ = "Blah"

p2 :: IO ()
p2 = print Blah
