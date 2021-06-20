module O20__Folding () where

-- Folds are catamorphisms.

-- `foldr` folds over a list in a right-associative manner.

_ = foldr (+) 0 [1, 2, 3]         -- `6`
_ = 1 + foldr (+) 0 [2, 3]        -- `6`
_ = 1 + 2 + foldr (+) 0 [3]       -- `6`
_ = 1 + 2 + 3 + foldr (+) 0 []    -- `6`

-- -----------------------------------------------------------------------------

-- In the following, undefined is part of the values.

_ = foldr (+) 0 [1, 2, 3, 4, undefined]                 -- Exception.
_ = foldr (+) 0 $ take 4 [1, 2, 3, 4, undefined]        -- `10`

-- In the following, undefined is part of the spine.

_ = foldr (+) 0 $ [1, 2, 3, 4] ++ undefined             -- Exception.
_ = foldr (+) 0 $ take 4 $ [1, 2, 3, 4] ++ undefined    -- `10`

-- -----------------------------------------------------------------------------

-- `length` will always evaluate the spine but not the values.

_ = length [1, 2, 3, 4, undefined]                      -- `5`
_ = length ([1, 2, 3, 4] ++ undefined)                  -- Exception.
_ = length (take 4 ([1, 2, 3, 4] ++ undefined))         -- `4`
_ = length $ take 2 $ take 4 $ [1, 2] ++ undefined      -- `2`

-- -----------------------------------------------------------------------------

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs

_ = myAny even [1..]    -- `True`

{-
_ = myAny even $ repeat 1    -- Results in an infinite loop.
-}


-- -----------------------------------------------------------------------------

-- There can be a function that does not force evaluation of values or spines.

_ = foldr (\_ _ -> 9001) 0 [1..5]                      -- `9001`
_ = foldr (\_ _ -> 9001) 0 [1, 2, 3, undefined]        -- `9001`
_ = foldr (\_ _ -> 9001) 0 $ [1, 2, 3] ++ undefined    -- `9001`
_ = foldr (\_ _ -> 9001) 0 [1, undefined]              -- `9001`
_ = foldr (\_ _ -> 9001) 0 [undefined, undefined]      -- `9001`

{-
_ = foldr (\_ _ -> 9001) 0 undefined    -- Exception.
-}


-- -----------------------------------------------------------------------------


_ = const 1 undefined           -- `1`
_ = (flip const) 1 undefined    -- Exception.
_ = (flip const) undefined 1    -- `1`

-- `foldr` recuses down the spine if it has to.

_ = foldr const 0 ([1..5] ++ undefined)            -- `1`
_ = foldr (flip const) 0 ([1..5] ++ undefined)     -- Exception.

-- `foldl` unconditionally evaluates the spine.

_ = foldl const 0 ([1..5] ++ undefined)            -- Exception.
_ = foldl (flip const) 0 ([1..5] ++ undefined)     -- Exception.

_ = foldl (\_ _ -> 5) 0 ([1..5] ++ undefined)      -- Exception.
_ = foldl (\_ _ -> 5) 0 ([1..5] ++ [undefined])    -- `5`


-- -----------------------------------------------------------------------------

_ = take 3 $ foldr (:) [] $ [1, 2, 3] ++ undefined           -- `[1, 2, 3]`
_ = take 3 $ foldl (flip (:)) [] $ [1, 2, 3] ++ undefined    -- Exception.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- For finite lists, `foldr` and `foldl` can be made to return the same result.

f = (:)
z = []
xs = [1..10]

_ = foldr f z xs                     -- `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`
_ = foldl (flip f) z (reverse xs)    -- `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`
_ = reverse $ foldl (flip f) z xs    -- `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`




-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------


-- `foldl'` is like `foldl` except it evaluates strictly instead of lazily.


-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

showPair x y = concat ["(", x, " + ", y, ")"]

-- `foldr` associates to the right.

_ = foldr showPair "0" (map show [1..4])    -- `"(1 + (2 + (3 + (4 + 0))))"`

_ = foldr (^) 2 [1..3]    -- `1`
-- (1 ^ (2 ^ (3 ^ 2)))
-- (1 ^ (2 ^ 9))
-- 1 ^ 512
-- 1

-- -----------------------------------------------------------------------------

-- `foldl` associates to the left.

_ = foldl showPair "0" (map show [1..4])    -- `"((((0 + 1) + 2) + 3) + 4)"`

_ = foldl (^) 2 [1..3]    -- `64`
-- ((2 ^ 1) ^ 2) ^ 3
-- (2 ^ 2) ^ 3
-- 4 ^ 3
-- 64

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Scans are similar to folds but they return a list of all the intermediate
-- stages of the folding.

_ = foldr (+) 0 [1..5]    -- `15`
_ = scanr (+) 0 [1..5]    -- `[15, 14, 12, 9, 5, 0]`

_ = foldl (+) 0 [1..5]    -- `15`
_ = scanl (+) 0 [1..5]    -- `[0, 1, 3, 6, 10, 15]`

-- -----------------------------------------------------------------------------

_ = scanr (+) 0 [1..3]    -- `[6, 5, 3, 0]`
-- [1 + (2 + (3 + 0)), 2 + (3 + 0), 3 + 0, 0]
-- [6, 5, 3, 0]

_ = scanl (+) 0 [1..3]    -- `[0, 1, 3, 6]`
-- [0, 0 + 1, (0 + 1) + 2, ((0 + 1) + 2) + 3]
-- [0, 1, 3, 6]


-- -----------------------------------------------------------------------------


myScanl :: (a -> b -> a) -> a -> [b] -> [a]
myScanl f q ls =
  q : (case ls of
        []   -> []
        x:xs -> myScanl f (f q x) xs)







-- -----------------------------------------------------------------------------


fibs = 1 : scanl (+) 1 fibs    -- Infinite list of the Fibonacci sequence.
_ = take 10 fibs               -- `[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]`




fac = scanl (*) 1 [1..]
_ = take 10 fac




-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------





_ = foldr (:) [] [1..3]           -- `[1, 2, 3]`
_ = foldl (flip (:)) [] [1..3]    -- `[3, 2, 1]`

{-
_ = foldl (:) [] [1..3]    -- Error.
-}




-- -----------------------------------------------------------------------------



_ = foldr const 0 [1..5]           -- `1`
_ = foldr (flip const) 0 [1..5]    -- `0`

_ = foldl const 0 [1..5]           -- `0`
_ = foldl (flip const) 0 [1..5]    -- `5`




-- -----------------------------------------------------------------------------


_ = foldr (:) [] (1 : 2 : 3 : [])                          -- `[1, 2, 3]`
_ = 1 : (2 : (3 : []))                                     -- `[1, 2, 3]`
_ = foldr (:) [] (1 : 2 : 3 : []) == 1 : (2 : (3 : []))    -- `True`

-- -----------------------------------------------------------------------------

-- Again, this type will be less
-- reusable than the one in GHC 7.10
-- and newer. Don't worry.

-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = if x == False then False else myAnd xs

-- direct recursion, using (&&)
myAnd1 :: [Bool] -> Bool
myAnd1 []     = True
myAnd1 (x:xs) = x && myAnd1 xs

-- fold, not point-free
-- in the folding function
myAnd2 :: [Bool] -> Bool
myAnd2 = foldr (\a b -> if a == False then False else b) True

-- fold, both myAnd and the folding
-- function are point-free now
myAnd3 :: [Bool] -> Bool
myAnd3 = foldr (&&) True

-- -----------------------------------------------------------------------------

myOr :: [Bool] -> Bool
myOr = foldr (||) False

_ = myOr [False, False, True, False, False]    -- `True`

-- -----------------------------------------------------------------------------

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr (\x acc -> acc || f x) False

_ = myAny even [1, 3, 5]    -- `False`
_ = myAny odd [1, 3, 5]     -- `True`

-- -----------------------------------------------------------------------------

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (== x)) False

_ = myElem 1 [1..10]    -- `True`
_ = myElem 1 [2..10]    -- `False`

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (== x)

_ = myElem' 1 [1..10]   -- `True`
_ = myElem' 1 [2..10]   -- `False`

-- -----------------------------------------------------------------------------

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

_ = myReverse "blah"    -- `"halb"`
_ = myReverse [1..5]    -- `[5, 4, 3, 2, 1]`

-- -----------------------------------------------------------------------------

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

_ = myMap (+ 1) [1, 2, 3]    -- `[2, 3, 4]`

-- -----------------------------------------------------------------------------

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

_ = myFilter even [1..10]    -- `[2, 4, 6, 8, 10]`

-- -----------------------------------------------------------------------------

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

_ = squish [[2, 3], [4], [5, 6, 7]]   -- `[2, 3, 4, 5, 6, 7]`

-- -----------------------------------------------------------------------------

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

_ = squishMap (\x -> [x, x]) [1, 2, 3]                 -- `[1, 1, 2, 2, 3, 3]`
_ = squishMap (\x -> [1, x, 3]) [2]                    -- `[1, 2, 3]`
_ = squishMap (\x -> "WO " ++ [x] ++ " OT ") "blah"    -- `"WO b OT WO l OT WO a OT WO h OT "`

-- -----------------------------------------------------------------------------

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

_ = squishAgain [[2, 3], [4], [5, 6, 7]]    -- `[2, 3, 4, 5, 6, 7]`

-- -----------------------------------------------------------------------------

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldl (\acc y -> if f acc y == GT then acc else y) (head xs) xs

_ = myMaximumBy (\_ _ -> GT) [1..10]    -- `1`
_ = myMaximumBy (\_ _ -> LT) [1..10]    -- `10`
_ = myMaximumBy compare [1..10]         -- `10`

-- -----------------------------------------------------------------------------

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldl (\acc y -> if f acc y == LT then acc else y) (head xs) xs

_ = myMinimumBy (\_ _ -> GT) [1..10]    -- `10`
_ = myMinimumBy (\_ _ -> LT) [1..10]    -- `1`
_ = myMinimumBy compare [1..10]         -- `1`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

Fold: A HoF that builds up the result of applying a given function to a list.
Catamorphism: A generalization of folds to arbitrary datatypes.
Tail call: A function application appearing at the end of another function.
Tail recursion: A function whose tail call references it.

-}
