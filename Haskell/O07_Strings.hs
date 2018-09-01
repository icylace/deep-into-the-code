module O07_Strings where

-- A character represents a unit of text.

_ = 'A'   -- `'A'`

-- A list of characters represents a sequence of text.

_ = ['A', 'B']    -- `"AB"`

-- Lists will be explored in detail later on.

-- A string value is syntactic sugar for a list of characters.
-- Strings are denoted by surrounding double quotes.

_ = "AB"    -- `"AB"`

-- An empty string has no characters and corresponds to the empty list, `[]`.

_ = ""    -- `""`

-- Strings can be concatenated, which means to join together.

-- One way to do this is to use the append operator, `++`.

_ = "Hello" ++ "world!"         -- `"Helloworld!"`
_ = "Hello" ++ "world!" ++ ""   -- `"Helloworld!"`

_ = "Hello" ++ " world!"                      -- `"Hello world!"`
_ = "Hello " ++ "world!"                      -- `"Hello world!"`
_ = "Hello" ++ "" ++ " " ++ "" ++ "world!"    -- `"Hello world!"`

_ = "Hello " ++ " world!"   -- `"Hello  World!"`

-- Another way to concatenate strings is to use the `concat` function.

_ = concat ["Hello world!"]                   -- `"Hello World!"`
_ = concat ["Hello", " world!"]               -- `"Hello World!"`
_ = concat ["Hello ", "world!"]               -- `"Hello World!"`
_ = concat ["Hello", " ", "world!"]           -- `"Hello World!"`
_ = concat ["Hello", "", " ", "", "world!"]   -- `"Hello World!"`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The append operator and `concat` function are not string-specific functions.
-- They work on lists generally.  Lists will be explored in more detail later
-- on but let's look at some other list functions useful for strings.

-- The "cons" operator, `:`, builds a list.  (Sidenote: cons in this context
-- might be short for construct but I'm not sure.)

_ = 'H' : "ello"                        -- `"Hello"`
_ = 'H' : 'e' : "llo"                   -- `"Hello"`
_ = 'H' : 'e' : 'l' : 'l' : 'o' : ""    -- `"Hello"`
_ = 'H' : 'e' : 'l' : 'l' : 'o' : []    -- `"Hello"`
_ = "H" : ["ello"]                      -- `["H", "ello"]`
_ = "He" : ["llo"]                      -- `["He", "llo"]`

-- The `head` function returns the head (the first element) of a list.

_ = head "Hello"          -- `'H'`
_ = head "H"              -- `'H'`
_ = head ["H", "ello"]    -- `"H"`
_ = head ["He", "llo"]    -- `"He"`

-- Attempting to use an empty string or more generally an empty list with
-- `head` will cause an exception.
--
--     head ""
--     head []
--

-- The `tail` function returns a list containing the tail (non-head elements)
-- of a given list.

_ = tail "Hello"          -- `"ello"`
_ = tail ["H", "ello"]    -- `["ello"]`
_ = tail "ab"             -- `"b"`
_ = tail "a"              -- `""`

-- Attempting to use an empty string or more generally an empty list with
-- `tail` will cause an exception.
--
--     tail ""
--     tail []
--

-- The `take` function returns a list of the specified number of elements
-- from the beginning of a given list.

_ = take 0 "Hello"    -- `""`
_ = take 1 "Hello"    -- `"H"`
_ = take 10 "Hello"   -- `"Hello"`

_ = take 0 ""     -- `""`
_ = take 1 ""     -- `""`
_ = take 10 ""    -- `""`

-- Attempting to use a negative number with `take` will cause an error.
--
--     take -1 ""
--

-- The `drop` function returns a list of elements remaining after a specified
-- number of elements has been ignored from a given list.

_ = drop 0 "Hello"    -- `"Hello"`
_ = drop 1 "Hello"    -- `"ello"`
_ = drop 10 "Hello"   -- `""`

_ = drop 0 ""     -- `""`
_ = drop 1 ""     -- `""`
_ = drop 10 ""    -- `""`

-- Attempting to use a negative number with `drop` will cause an error.
--
--     drop -1 ""
--

-- The list index (subscript) operator, `!!`, returns the element at the
-- specified position in a list.

_ = "Hello" !! 0    -- `'H'`
_ = "Hello" !! 3    -- `'l'`
_ = "Hello" !! 4    -- `'o'`

-- `head` and `(!! 0)` behave the same way.

-- Attempting to use a negative index position with the subscript operator
-- will cause an error.
--
--     "Hello" !! -1
--

-- Attempting to use an out-of-bounds index position with the subscript
-- operator will cause an exception.
--
--     "Hello" !! 5
--     "" !! 0
--

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- To print strings to the screen, we need to make use of our special `main`
-- function which lets us conduct I/O (input/output) operations.

printStrings :: IO ()
printStrings = do

  print "Hello world!"
  -- Displays `"Hello world!"`.

  putStrLn "Hello world!"
  -- Displays `Hello world!`.

  putStr "Hello world!"
  -- Displays `Hello world!` without a carriage return.

-- `do` notation was used to make using a sequence of print-related
-- functions more convenient.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Character: A unit of text.
-- String: A sequence of characters.
-- Empty string: A string with no characters.
-- Concatenation: The joining together of sequences of values.

-- Partial function:
-- Total function:
-- Unsafe function:
-- Error:
-- Exception:
-- List: An ordered collection of similarly typed elements.
-- Head: The first element of a list.
-- Tail: The non-head elements of a list.

-- `do` notation: Syntatic sugar for sequencing function evaluations.
-- I/O (IO): Input/output.
