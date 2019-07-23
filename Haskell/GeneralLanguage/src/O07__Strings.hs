module O07__Strings () where

-- A character represents a unit of text.

_ = 'A'    -- `'A'`

-- A list of characters represents a sequence of text.

_ = ['A', 'B']    -- `"AB"`

-- Lists will be explored in detail later on.

-- A string value is syntactic sugar for a list of characters. Strings are
-- denoted by surrounding double quotes.

_ = "AB"    -- `"AB"`

-- An empty string has no characters and corresponds to the empty list, `[]`.

_ = ""    -- `""`

-- Strings can be concatenated, which means to join together.

-- One way to do this is to use the append operator, `++`.

_ = "Hello" ++ "world!"          -- `"Helloworld!"`
_ = "Hello" ++ "world!" ++ ""    -- `"Helloworld!"`

_ = "Hello" ++ " world!"                      -- `"Hello world!"`
_ = "Hello " ++ "world!"                      -- `"Hello world!"`
_ = "Hello" ++ "" ++ " " ++ "" ++ "world!"    -- `"Hello world!"`

_ = "Hello " ++ " world!"    -- `"Hello  World!"`

-- Another way to concatenate strings is to use `concat`.

_ = concat ["Hello world!"]                    -- `"Hello World!"`
_ = concat ["Hello", " world!"]                -- `"Hello World!"`
_ = concat ["Hello ", "world!"]                -- `"Hello World!"`
_ = concat ["Hello", " ", "world!"]            -- `"Hello World!"`
_ = concat ["Hello", "", " ", "", "world!"]    -- `"Hello World!"`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The append operator and `concat` function are not string-specific functions.
-- They work on lists generally. Lists will be explored in more detail later
-- on but let's look at some other list functions useful for strings.

-- The "cons" operator (short for "construct"), `:`, builds a list.

_ = 'H' : "ello"                        -- `"Hello"`
_ = 'H' : 'e' : "llo"                   -- `"Hello"`
_ = 'H' : 'e' : 'l' : 'l' : 'o' : ""    -- `"Hello"`
_ = 'H' : 'e' : 'l' : 'l' : 'o' : []    -- `"Hello"`
_ = "H" : ["ello"]                      -- `["H", "ello"]`
_ = "He" : ["llo"]                      -- `["He", "llo"]`

-- `head` the head (the first element) of a list.

_ = head "Hello"          -- `'H'`
_ = head "H"              -- `'H'`
_ = head ["H", "ello"]    -- `"H"`
_ = head ["He", "llo"]    -- `"He"`

{-
_ = head ""    -- Exception.
_ = head []    -- Exception.
-}

-- `tail` returns a given list without its first element.

_ = tail "Hello"          -- `"ello"`
_ = tail ["H", "ello"]    -- `["ello"]`
_ = tail "ab"             -- `"b"`
_ = tail "a"              -- `""`

{-
_ = tail ""    -- Exception.
_ = tail []    -- Exception.
-}

-- `take` returns a number of elements from the beginning of a given list.

_ = take 0 "Hello"     -- `""`
_ = take 1 "Hello"     -- `"H"`
_ = take 10 "Hello"    -- `"Hello"`

_ = take 0 ""     -- `""`
_ = take 1 ""     -- `""`
_ = take 10 ""    -- `""`

{-
_ = take -1 ""    -- Exception.
-}

-- The `drop` function returns a list of elements remaining after a specified
-- number of elements has been ignored from a given list.

_ = drop 0 "Hello"     -- `"Hello"`
_ = drop 1 "Hello"     -- `"ello"`
_ = drop 10 "Hello"    -- `""`

_ = drop 0 ""     -- `""`
_ = drop 1 ""     -- `""`
_ = drop 10 ""    -- `""`

{-
_ = drop -1 ""    -- Exception.
-}

-- The list index (subscript) operator, `!!`, returns the element at the
-- specified position in a list.

-- `head` and `(!! 0)` behave the same way.

_ = "Hello" !! 0    -- `'H'`
_ = "Hello" !! 3    -- `'l'`
_ = "Hello" !! 4    -- `'o'`

{-
_ = "Hello" !! -1    -- Exception.
_ = "Hello" !! 5     -- Exception.
_ = "" !! 0          -- Exception.
-}

-- -----------------------------------------------------------------------------

_ = "ab" <> "cd"    -- "abcd"



-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- To print strings to the screen, we need to make use of our special `main`
-- function which lets us conduct I/O (input/output) operations.

printStrings :: IO ()
printStrings = do
  print "Hello world!"       -- Displays `"Hello world!"`.
  putStrLn "Hello world!"    -- Displays `Hello world!`.
  putStr "Hello world!"      -- Displays `Hello world!` without a carriage return.

-- `do` notation was used to make using a sequence of print-related functions
-- more convenient.

-- We use `do` inside functions that return `IO` in order to sequence side
-- effects in a convenient syntax.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

Character: A unit of text.
String: A sequence of characters.
Empty string: A string with no characters.
Concatenation: The joining together of sequences of values.

Unsafe function:
Error:
Exception:
List: An ordered collection of similarly typed elements.
Head: The first element of a list.
Tail: The non-head elements of a list.

`do` notation: Syntatic sugar for sequencing function evaluations.
I/O (IO): Input/output.

-}
