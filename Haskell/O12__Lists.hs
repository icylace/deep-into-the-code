module O12__Lists where

-- We import character functions to use later.

import Data.Char

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




-- `length` is strict in the spine but not the values.  `length [1, 2, 3]`
-- would force evaluation of the entire spine without accompanying
-- strictness in the values.

--   :
--  / \
-- _   :
--    / \
--   _   :
--      / \
--     _  []

-- We can see this if we use `length` with a list that contains an `undefined`.

_ = length [1, undefined, 3]    -- `3`

--       :       <-|
--      / \        |
-- |-> _   :     <-|
-- |      / \      | These got evaluated (forced)
-- |->   _   :   <-|
-- |        / \    |
-- |->     _  [] <-|
-- |
-- | These did not

-- If the spine of a list has a bottom, `length` won't work.

_ = length $ [1] ++ undefined ++ [3]    -- Throws an exception.


-- -----------------------------------------------------------------------------

-- It's possible to have a function that forces both the spine and the values.

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

-- The addition operator is strict in both its arguments, so that will force
-- evaluation on every recursive step taken.

_ = mySum [1..5]    -- `15`

-- An illustration of the calculation:
--
-- 1 + (2 + (3 + (4 + (5 + 0))))
-- 1 + (2 + (3 + (4 + 5)))
-- 1 + (2 + (3 + 9))
-- 1 + (2 + 12)
-- 1 + 14
-- 15

-- -----------------------------------------------------------------------------

_ = [x^y | x <- [1..5], y <- [2, undefined]]            -- Throws an exception.
_ = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]   -- `[1]`
_ = sum [1, undefined, 3]                               -- Throws an exception.
_ = length [1, 2, undefined]                            -- `3`
_ = length $ [1, 2, 3] ++ undefined                     -- Throws an exception.
_ = take 1 $ filter even [1, 2, 3, undefined]           -- `[2]`
_ = take 1 $ filter even [1, 3, undefined]              -- Throws an exception.
_ = take 1 $ filter odd [1, 3, undefined]               -- `[1]`
_ = take 2 $ filter odd [1, 3, undefined]               -- `[1,3]`
_ = take 3 $ filter odd [1, 3, undefined]               -- Throws an exception.

-- -----------------------------------------------------------------------------













-- Remember that an expression cannot be in normal form or
-- weak head normal form if the outermost part of the expression
-- isn't a data constructor. It can't be in normal form if any part
-- of the expression is unevaluated.

_ = [1, 2, 3, 4, 5]   -- `[1,2,3,4,5]`
-- Normal form and WHNF.

{-
_ = 1 : 2 : 3 : 4 : _   -- Throws an exception.
-- WHNF.
-}

_ = enumFromTo 1 10   -- `[1,2,3,4,5,6,7,8,9,10]`
-- Neither normal form nor WHNF.

_ = length [1, 2, 3, 4, 5]    -- `5`
-- Neither normal form nor WHNF.

_ = sum (enumFromTo 1 10)   -- `55`
-- Neither normal form nor WHNF.

_ = ['a'..'m'] ++ ['n'..'z']    -- `"abcdefghijklmnopqrstuvwxyz"`
-- Neither normal form nor WHNF.

{-
_ = (_, 'b')    -- Throws an exception.
-- WHNF.
-}





-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- `map` applies a given function to each element of a list.

_ = map (+ 1) [1, 2, 3, 4]    -- `[2,3,4,5]`
_ = map (1 -) [1, 2, 3, 4]    -- `[0,-1,-2,-3]`
_ = map id [1, 2, 3]          -- `[1,2,3]`

-- `fmap` is similar to map but it can also apply to other types of data.

_ = fmap (+ 1) [1, 2, 3, 4]   -- `[2,3,4,5]`
_ = fmap (2 *) [1, 2, 3, 4]   -- `[2,4,6,8]`
_ = fmap id [1, 2, 3]         -- `[1,2,3]`

-- -----------------------------------------------------------------------------

_ = map (+ 1) [1, 2, undefined]             -- Throws an exception.
_ = take 2 $ map (+ 1) [1, 2, undefined]    -- `[2,3]`

-- -----------------------------------------------------------------------------

_ = map fst [(2, 3), (4, 5), (6, 7), (8, 9)]          -- `[2,4,6,8]`
_ = fmap fst [(2, 3), (4, 5), (6, 7), (8, 9)]         -- `[2,4,6,8]`
_ = map (take 3) [[1..5], [1..5], [1..5]]             -- `[[1,2,3],[1,2,3],[1,2,3]]`
_ = map (\x -> if x == 3 then (-x) else x) [1..10]    -- `[1,2,-3,4,5,6,7,8,9,10]`


-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- A predicate is a function that returns a boolean value based on a condition.

-- `filter` gathers elements from a list that pass a given predicate.

_ = filter even [1..10]                     -- `[2,4,6,8,10]`
_ = filter (\x -> (rem x 2) == 0) [1..20]   -- `[2,4,6,8,10,12,14,16,18,20]`

_ = filter (== 'a') "abracadabra"                 -- `"aaaaa"`
_ = filter (\x -> elem x "aeiou") "abracadabra"   -- `"aaaaa"`
_ = [x | x <- "abracadabra", elem x "aeiou"]      -- `"aaaaa"`














myFilter :: String -> [String]
myFilter [] = []
myFilter xs = filter notArticle . go $ xs
  where
    go ys =
      (takeWhile (/= ' ') ys)
      : (myFilter $ dropWhile (== ' ')
                  . dropWhile (/= ' ') $ ys)
    notArticle x = x /= "the" && x /= "a" && x /= "an"

_ = myFilter "the brown dog was a goof"   -- `["brown","dog","was","goof"]`






myFilter' :: String -> [String]
myFilter' = filter notArticle . words
  where notArticle x = x /= "the" && x /= "a" && x /= "an"

_ = myFilter' "the brown dog was a goof"    -- `["brown","dog","was","goof"]`





myFilter'' :: String -> [String]
myFilter'' = filter (\x -> notElem x ["the", "a", "an"]) . words

_ = myFilter'' "the brown dog was a goof"   -- `["brown","dog","was","goof"]`







myFilter2 :: String -> [String]
myFilter2 = filter (flip notElem ["the", "a", "an"]) . words

_ = myFilter2 "the brown dog was a goof"   -- `["brown","dog","was","goof"]`




















-- -----------------------------------------------------------------------------














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

-- Zipping is the process of associating of values from multiple lists into a
-- single list.

-- `zip` takes a couple lists and zips them together.

_ = zip [1, 2, 3] [4, 5, 6]   -- `[(1,4),(2,5),(3,6)]`

-- `zip` proceeds while both lists have values remaining to combine.

_ = zip [1, 2] [4, 5, 6]    -- `[(1,4),(2,5)]`
_ = zip [1, 2, 3] [4]       -- `[(1,4)]`
_ = zip [] [1, 2, 3]        -- `[]`

-- `zip` can combine lists of different types.

_ = zip ['a'] [1..10000000]   -- `[('a',1)]`
_ = zip [1..100] ['a'..'c']   -- `[(1,'a'),(2,'b'),(3,'c')]`

-- -----------------------------------------------------------------------------

-- `unzip` creates a tuple of lists by separating values from a list of tuples.

_ = unzip [(1, 4), (2, 5), (3, 6)]    -- `([1,2,3],[4,5,6])`

-- `unzip` can be used to recover lists as they were before being zipped.

_ = unzip $ zip [1, 2, 3] [4, 5, 6]   -- `([1,2,3],[4,5,6])`

-- Recovering lists won't always completely work because of how `zip` works.

_ = unzip $ zip [1, 2] [4, 5, 6]    -- `([1,2],[4,5])`

-- -----------------------------------------------------------------------------

-- `zipWith` applies a function to the values of a couple lists in pairwise
-- fashion and saves the results in a list.

_ = zipWith (+) [1, 2, 3] [10, 11, 12]    -- `[11,13,15]`
_ = zipWith (*) [1, 2, 3] [10, 11, 12]    -- `[10,22,36]`
_ = zipWith (==) ['a'..'f'] ['a'..'m']    -- `[True,True,True,True,True,True]`
_ = zipWith max [10, 5, 34] [6, 8, 12]    -- `[10,8,34]`

-- `zipWith (,)` emulates the behavior of `zip`.

_ = zip [10, 5, 34] [6, 8, 12]            -- `[(10,6),(5,8),(34,12)]`
_ = zipWith (,) [10, 5, 34] [6, 8, 12]    -- `[(10,6),(5,8),(34,12)]`






-- -----------------------------------------------------------------------------



myZip :: [a] -> [b] -> [(a, b)]
myZip []     _      = []
myZip _      []     = []
myZip (x:xs) (y:ys) = [(x, y)] ++ myZip xs ys

_ = myZip [1, 2, 3] [4, 5, 6]   -- `[(1,4),(2,5),(3,6)]`
_ = myZip [1, 2] [4, 5, 6]      -- `[(1,4),(2,5)]`
_ = myZip [1, 2, 3] [4]         -- `[(1,4)]`
_ = myZip [] [1, 2, 3]          -- `[]`
_ = myZip ['a'] [1..10000000]   -- `[('a',1)]`
_ = myZip [1..100] ['a'..'c']   -- `[(1,'a'),(2,'b'),(3,'c')]`

-- -----------------------------------------------------------------------------

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ []     _      = []
myZipWith _ _      []     = []
myZipWith f (x:xs) (y:ys) = [f x y] ++ myZipWith f xs ys

_ = myZipWith (+) [1, 2, 3] [10, 11, 12]    -- `[11,13,15]`
_ = myZipWith (*) [1, 2, 3] [10, 11, 12]    -- `[10,22,36]`
_ = myZipWith (==) ['a'..'f'] ['a'..'m']    -- `[True,True,True,True,True,True]`
_ = myZipWith max [10, 5, 34] [6, 8, 12]    -- `[10,8,34]`
_ = myZipWith (,) [10, 5, 34] [6, 8, 12]    -- `[(10,6),(5,8),(34,12)]`

-- -----------------------------------------------------------------------------

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)

_ = myZip' [1, 2, 3] [4, 5, 6]    -- `[(1,4),(2,5),(3,6)]`
_ = myZip' [1, 2] [4, 5, 6]       -- `[(1,4),(2,5)]`
_ = myZip' [1, 2, 3] [4]          -- `[(1,4)]`
_ = myZip' [] [1, 2, 3]           -- `[]`
_ = myZip' ['a'] [1..10000000]    -- `[('a',1)]`
_ = myZip' [1..100] ['a'..'c']    -- `[(1,'a'),(2,'b'),(3,'c')]`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------








-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

capitalize :: String -> String
capitalize (x:xs) = [toUpper x] ++ xs

_ = capitalize "woot"   -- `"Woot"`

-- -----------------------------------------------------------------------------

allCaps :: String -> String
allCaps []     = []
allCaps (x:xs) = toUpper x : allCaps xs

_ = allCaps "woot"    -- `"WOOT"`

-- -----------------------------------------------------------------------------

headCap :: String -> Char
headCap = toUpper . head

_ = headCap "woot"    -- `'W'`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------











myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f []     = False
myAny f (x:xs) = f x || myAny f xs

_ = myAny even [1, 3, 5]    -- `False`
_ = myAny odd [1, 3, 5]     -- `True`

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem x (y:ys) = x == y || myElem x ys

_ = myElem 1 [1..10]    -- `True`
_ = myElem 1 [2..10]    -- `False`

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (== x)

_ = myElem' 1 [1..10]   -- `True`
_ = myElem' 1 [2..10]   -- `False`

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

_ = myReverse "blah"    -- `"halb"`
_ = myReverse [1..5]    -- `[5,4,3,2,1]`

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

_ = squish [[5, 4], [3, 2], [1]]    -- `[5,4,3,2,1]`




squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

_ = squishMap (\x -> [1, x, 3]) [2]                   -- `[1,2,3]`
_ = squishMap (\x -> "WO " ++ [x] ++ " HOO ") "123"   -- `"WO 1 HOO WO 2 HOO WO 3 HOO "`





squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

_ = squishAgain [[5, 4], [3, 2], [1]]   -- `[5,4,3,2,1]`






myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x]    = x
myMaximumBy f (x:xs) = if f x curMax == GT then x else curMax
  where curMax = myMaximumBy f xs

_ = myMaximumBy compare [1, 53, 9001, 10]   -- `9001`







myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x]    = x
myMinimumBy f (x:xs) = if f x curMin == LT then x else curMin
  where curMin = myMinimumBy f xs

_ = myMinimumBy compare [1, 53, 9001, 10]   -- `1`








myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

_ = myMaximum [1, 53, 9001, 10]   -- `9001`







myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

_ = myMinimum [1, 53, 9001, 10]   -- `1`





-- -----------------------------------------------------------------------------


-- `repeat` generates an infinite list of a given value.

_ = repeat 1              -- Results in an infinite list of `1` continually repeated.
_ = take 4 $ repeat 1     -- `[1,1,1,1]`
_ = take 10 $ repeat 1    -- `[1,1,1,1,1,1,1,1,1,1]`
_ = drop 10 $ repeat 1    -- Results in an infinite list of `1` continually repeated.








-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Cons: To create a list by prepending a value to the beginning of a list.
-- Cons'ing: The performing of a cons.
-- Cons cell: In Haskell, a data constructor product of a value and a list.
-- Spine: The connective structure that holds a collection of values together.
-- List comprehension: A generated list based on one or more lists.
-- Generator: A list used as input for a comprehension.
-- Zipping: The associating of values from multiple lists into a single list.
