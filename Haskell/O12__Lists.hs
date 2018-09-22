module O12__Lists where

-- We import concrete integer types to use later.

import GHC.Int




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
-- can work with different types as long as those types are used with it
-- correctly.






-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The cons operator can be used to pattern match lists.

myHead :: [a] -> a
myHead (x:_) = x

_ = myHead [1, 2, 3]    -- `1`

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- -----------------------------------------------------------------------------

myTail :: [a] -> [a]
myTail (_:xs) = xs

_ = myTail [1, 2, 3]    -- `[2,3]`

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------











_ = [1, 2, 3]

--   : <------|
--  / \       |
-- _   : <----| This is the "spine"
--    / \     |
--   _   : <--|
--      / \
--     _  []



-- Bracket notation for lists is syntactic sugar.

_ = [1, 2, 3, 4]                -- `[1,2,3,4]`
_ = 1 : 2 : 3 : 4 : []          -- `[1,2,3,4]`
_ = 1 : (2 : (3 : (4 : [])))    -- `[1,2,3,4]`

_ = [1, 2, 3] ++ [4]            -- `[1,2,3,4]`
_ = 1 : 2 : 3 : [] ++ 4 : []    -- `[1,2,3,4]`

-- Range syntax, also syntactic sugar, can create a list of several values.

_ = [1..10]           -- `[1,2,3,4,5,6,7,8,9,10]`
_ = enumFromTo 1 10   -- `[1,2,3,4,5,6,7,8,9,10]`


_ = [1..4]      -- `[1,2,3,4]`
_ = [1.. 4]     -- `[1,2,3,4]`
_ = [1 ..4]     -- `[1,2,3,4]`
_ = [1 .. 4]    -- `[1,2,3,4]`





_ = [1, 2..10]              -- `[1,2,3,4,5,6,7,8,9,10]`
_ = enumFromThenTo 1 2 10   -- `[1,2,3,4,5,6,7,8,9,10]`


_ = [18, 44..300]               -- `[18,44,70,96,122,148,174,200,226,252,278]`
_ = enumFromThenTo 18 44 300    -- `[18,44,70,96,122,148,174,200,226,252,278]`




_ = ['t'..'z']            -- `"tuvwxyz"`
_ = enumFromTo 't' 'z'    -- `"tuvwxyz"`





_ = enumFromTo 1 1    -- `[1]`
_ = enumFromTo 1 3    -- `[1,2,3]`
_ = enumFromTo 3 1    -- `[]`
_ = enumFromTo 3 3    -- `[3]`








-- `take` returns a number of elements from the beginning of a given list.

_ = take 7 ['a'..'z']   -- `"abcdefg"`
_ = take 3 [1..10]      -- `[1,2,3]`
_ = take 3 []           -- `[]`

_ = enumFrom 10   -- Results in an infinite list of integers starting from 10.

-- You can take from an infinite list to get a finite list.

_ = take 10 $ enumFrom 10   -- `[10,11,12,13,14,15,16,17,18,19]`






-- The `drop` function returns a list of elements remaining after a specified
-- number of elements has been ignored from a given list.

_ = drop 5 [1..10]              -- `[6,7,8,9,10]`
_ = drop 8 ['a'..'z']           -- `"ijklmnopqrstuvwxyz"`
_ = drop 4 []                   -- `[]`
_ = drop 2 $ enumFromTo 10 20   -- `[12,13,14,15,16,17,18,19,20]`





-- `splitAt` cuts a list into two parts at a specified element and makes a tuple
-- of a couple lists.

_ = splitAt 5 [1..10]             -- `([1,2,3,4,5],[6,7,8,9,10])`
_ = splitAt 10 ['a'..'z']         -- `("abcdefghij","klmnopqrstuvwxyz")`
_ = splitAt 5 []                  -- `([],[])`
_ = splitAt 3 $ enumFromTo 5 15   -- `([5,6,7],[8,9,10,11,12,13,14,15])`






-- `takeWhile` takes list elements until an element fails its condition.
-- takeWhile :: (a -> Bool) -> [a] -> [a]

_ = takeWhile (< 3) [1..10]             -- `[1,2]`
_ = takeWhile (< 8) $ enumFromTo 5 15   -- `[5,6,7]`
_ = takeWhile (> 6) [1..10]             -- `[]`
_ = takeWhile (== 'a') "abracadabra"    -- `"a"`







-- `dropWhile` ignores list elements until an element fails its condition.
-- dropWhile :: (a -> Bool) -> [a] -> [a]

_ = dropWhile (< 3) [1..10]             -- `[3,4,5,6,7,8,9,10]`
_ = dropWhile (< 8) $ enumFromTo 5 15   -- `[8,9,10,11,12,13,14,15]`
_ = dropWhile (> 6) [1..10]             -- `[1,2,3,4,5,6,7,8,9,10]`
_ = dropWhile (== 'a') "abracadabra"    -- `"bracadabra"`












myWords :: String -> [String]
myWords "" = []
myWords x  = [takeWhile (/= ' ') x] ++ (myWords $ dropWhile (== ' ') $ dropWhile (/= ' ') x)

_ = myWords "sheryl wants fun"    -- `["sheryl","wants","fun"]`





-- -----------------------------------------------------------------------------

-- A list comprehension is a means of generating a list from another list or
-- lists.

-- A list called the generator is used as input into the comprehension in order
-- to generate the output list.

_ = [x^2 | x <- [1..10]]    -- `[1,4,9,16,25,36,49,64,81,100]`

-- -----------------------------------------------------------------------------

-- A list comprehension can use predicates to limit the elements drawn from the
-- generator list.

_ = [x^2 | x <- [1..10], rem x 2 == 0]    -- `[4,16,36,64,100]`

-- -----------------------------------------------------------------------------

-- A list comprehension can have multiple generators.

-- When there are multiple generators, the right-most generator will be
-- exhausted first, the second right-most, and so on.

_ = [x^y | x <- [1..5], y <- [2, 3]]    -- `[1,1,4,8,9,27,16,64,25,125]`

-- -----------------------------------------------------------------------------

-- Multiple generators can be used alongside predicates.

_ = [x^y | x <- [1..5], y <- [2, 3], x^y < 20]    -- `[1,1,4,8,9,16]`

-- -----------------------------------------------------------------------------

-- A list comprehension with multiple generators can be used to group elements
-- from different lists together into tuples.

_ = [(x, y) | x <- [1, 2, 3], y <- [6, 7]]
-- `[(1,6),(1,7),(2,6),(2,7),(3,6),(3,7)]`

_ = [(x, y) | x <- [1, 2, 3], y <- ['a', 'b']]
-- `[(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]`

-- -----------------------------------------------------------------------------

-- Lists generated from a list comprehension, like regular lists, can be used as
-- a generator for another list comprehension.

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..10]]

_ = [(x, y) | x <- mySqr, y <- [1..3], x < 4]   -- `[(1,1),(1,2),(1,3)]`

-- -----------------------------------------------------------------------------




-- Multiple predicates can be used to affect the same or different generators.

_ = [(x, y) | x <- mySqr, y <- mySqr, x < 30, x > 2, y > 70]
-- `[(4,81),(4,100),(9,81),(9,100),(16,81),(16,100),(25,81),(25,100)]`






-- -----------------------------------------------------------------------------


-- As with any other list, list comprehensions can be chained together with
-- other list operations.

_ = take 5 [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- `[(1,64),(1,81),(1,100),(4,64),(4,81)]`









-- `elem` checks if an element is a member of a list.

_ = elem 'a' "abracadabra"    -- `True`
_ = elem 'a' "Julie"          -- `False`




_ = [x | x <- "Three Letter Acronym", elem x ['A'..'Z']]    -- `TLA`






acro :: String -> String
acro xs = [x | x <- xs, elem x ['A'..'Z']]

_ = acro "Self Contained Underwater Breathing Apparatus"    -- `"SCUBA"`
_ = acro "National Aeronautics and Space Administration"    -- `"NASA"`


myString :: String -> String
myString xs = [x | x <- xs, elem x "aeiou"]

_ = myString "Self Contained Underwater Breathing Apparatus"
-- `"eoaieeaeeaiaau"`

_ = myString "National Aeronautics and Space Administration"
-- `"aioaeoauiaaeiiaio"`


mySquare = [x^2 | x <- [1..5]]
myCube   = [y^3 | y <- [1..5]]

_ = [(x, y) | x <- mySquare, y <- myCube]
-- `[(1,1),(1,8),(1,27),(1,64),(1,125),(4,1),(4,8),(4,27),(4,64),(4,125),(9,1),(9,8),(9,27),(9,64),(9,125),(16,1),(16,8),(16,27),(16,64),(16,125),(25,1),(25,8),(25,27),(25,64),(25,125)]`

_ = [(x, y) | x <- mySquare, y <- myCube, x < 50, y < 50]
-- `[(1,1),(1,8),(1,27),(4,1),(4,8),(4,27),(9,1),(9,8),(9,27),(16,1),(16,8),(16,27),(25,1),(25,8),(25,27)]`

_ = length [(x, y) | x <- mySquare, y <- myCube, x < 50, y < 50]
-- `15`





-- A list generated from a range with no explicit upper bound can be thought of
-- as an infinite list.

_ = [1..]   -- Results in an infinite list of natural numbers.

-- Technically, however, a supposed infinite list is actually a list implicitly
-- bounded by the maximum value of the list's contained enumerated type.

_ = [maxBound :: Int]             -- `[9223372036854775807]`
_ = [(maxBound :: Int)..]         -- `[9223372036854775807]`
_ = [(maxBound :: Int) - 2..]     -- `[9223372036854775805,9223372036854775806,9223372036854775807]`
_ = [(maxBound :: Int8) - 5..]    -- `[122,123,124,125,126,127]`
_ = [(maxBound :: Char)..]        -- `\1114111`




-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Cons cell: The result of recursively prepending a value to lists.
-- Spine: The connective structure that holds cons cells togerth and in place.
-- List comprehension: A generated list based on one or more lists.
-- Generator: A list used as input for a comprehension.
