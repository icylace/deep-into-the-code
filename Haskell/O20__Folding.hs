module O20__Folding where

-- `foldr` folds over a list in a right-associative manner.

_ = foldr (+) 0 [1, 2, 3]         -- `6`
_ = 1 + foldr (+) 0 [2, 3]        -- `6`
_ = 1 + 2 + foldr (+) 0 [3]       -- `6`
_ = 1 + 2 + 3 + foldr (+) 0 []    -- `6`

-- -----------------------------------------------------------------------------

-- In the following, undefined is part of the values.

_ = foldr (+) 0 [1, 2, 3, 4, undefined]                 -- Throws an exception.
_ = foldr (+) 0 $ take 4 [1, 2, 3, 4, undefined]        -- `10`

-- In the following, undefined is part of the spine.

_ = foldr (+) 0 $ [1, 2, 3, 4] ++ undefined             -- Throws an exception.
_ = foldr (+) 0 $ take 4 $ [1, 2, 3, 4] ++ undefined    -- `10`

-- -----------------------------------------------------------------------------

-- `length` will always evaluate the spine but not the values.

_ = length [1, 2, 3, 4, undefined]                    -- `5`
_ = length ([1, 2, 3, 4] ++ undefined)                -- Throws an exception.
_ = length (take 4 ([1, 2, 3, 4] ++ undefined))       -- `4`
_ = length $ take 2 $ take 4 $ [1, 2] ++ undefined    -- `2`

-- -----------------------------------------------------------------------------

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs

_ = myAny even [1..]    -- `True`

{-
_ = myAny even $ repeat 1   -- Results in an infinite loop.
-}


-- -----------------------------------------------------------------------------

-- There can be a function that does not force evaluation of values or spines.

_ = foldr (\_ _ -> 9001) 0 [1..5]                     -- `9001`
_ = foldr (\_ _ -> 9001) 0 [1, 2, 3, undefined]       -- `9001`
_ = foldr (\_ _ -> 9001) 0 $ [1, 2, 3] ++ undefined   -- `9001`
_ = foldr (\_ _ -> 9001) 0 [1, undefined]             -- `9001`
_ = foldr (\_ _ -> 9001) 0 [undefined, undefined]     -- `9001`

{-
_ = foldr (\_ _ -> 9001) 0 undefined    -- Throws an exception.
-- Results in a compiler error.
-}


-- -----------------------------------------------------------------------------


_ = const 1 undefined           -- `1`
_ = (flip const) 1 undefined    -- Throws an exception.
_ = (flip const) undefined 1    -- `1`

-- `foldr` recuses down the spine if it has to.

_ = foldr const 0 ([1..5] ++ undefined)           -- `1`
_ = foldr (flip const) 0 ([1..5] ++ undefined)    -- Throws an exception.

-- `foldl` unconditionally evaluates the spine.

_ = foldl const 0 ([1..5] ++ undefined)           -- Throws an exception.
_ = foldl (flip const) 0 ([1..5] ++ undefined)    -- Throws an exception.

_ = foldl (\_ _ -> 5) 0 ([1..5] ++ undefined)     -- Throws an exception.
_ = foldl (\_ _ -> 5) 0 ([1..5] ++ [undefined])   -- `5`


-- -----------------------------------------------------------------------------

_ = take 3 $ foldr (:) [] $ [1, 2, 3] ++ undefined          -- `[1,2,3]`
_ = take 3 $ foldl (flip (:)) [] $ [1, 2, 3] ++ undefined   -- Throws an exception.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- For finite lists, `foldr` and `foldl` can be made to return the same result.

f = (:)
z = []
xs = [1..10]

_ = foldr f z xs                    -- `[1,2,3,4,5,6,7,8,9,10]`
_ = foldl (flip f) z (reverse xs)   -- `[1,2,3,4,5,6,7,8,9,10]`
_ = reverse $ foldl (flip f) z xs   -- `[1,2,3,4,5,6,7,8,9,10]`




-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- `foldl'` is similar to `foldl` except that it evaluates strictly instead of
-- evaluates lazily.

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
_ = scanr (+) 0 [1..5]    -- `[15,14,12,9,5,0]`

_ = foldl (+) 0 [1..5]    -- `15`
_ = scanl (+) 0 [1..5]    -- `[0,1,3,6,10,15]`

-- -----------------------------------------------------------------------------

_ = scanr (+) 0 [1..3]    -- `[6,5,3,0]`
-- [1 + (2 + (3 + 0)), 2 + (3 + 0), 3 + 0, 0]
-- [6, 5, 3, 0]

_ = scanl (+) 0 [1..3]    -- `[0,1,3,6]`
-- [0, 0 + 1, (0 + 1) + 2, ((0 + 1) + 2) + 3]
-- [0, 1, 3, 6]


-- -----------------------------------------------------------------------------


myScanl :: (a -> b -> a) -> a -> [b] -> [a]
myScanl f q ls =
  q : (case ls of
        []   -> []
        x:xs -> myScanl f (f q x) xs)


-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------





_ = foldr (:) [] [1..3]           -- `[1,2,3]`
_ = foldl (flip (:)) [] [1..3]    -- `[3,2,1]`

{-
_ = foldl (:) [] [1..3]   -- Results in a compiler error.
-}




-- -----------------------------------------------------------------------------



_ = foldr const 0 [1..5]          -- `1`
_ = foldr (flip const) 0 [1..5]   -- `0`

_ = foldl const 0 [1..5]          -- `0`
_ = foldl (flip const) 0 [1..5]   -- `5`







-- -----------------------------------------------------------------------------



-- Folds are catamorphisms.



-- -----------------------------------------------------------------------------


_ = foldr (:) [] (1 : 2 : 3 : [])                         -- `[1,2,3]`
_ = 1 : (2 : (3 : []))                                    -- `[1,2,3]`
_ = foldr (:) [] (1 : 2 : 3 : []) == 1 : (2 : (3 : []))   -- `True`




-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Tail call:
