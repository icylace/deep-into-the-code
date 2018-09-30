module O20__Folding where

-- `foldr` folds over a list in a right-associative manner.

_ = foldr (+) 0 [1, 2, 3]         -- `6`
_ = 1 + foldr (+) 0 [2, 3]        -- `6`
_ = 1 + 2 + foldr (+) 0 [3]       -- `6`
_ = 1 + 2 + 3 + foldr (+) 0 []    -- `6`

-- -----------------------------------------------------------------------------

_ = foldr (+) 0 [1, 2, 3, 4, undefined]                 -- Throws an exception.
_ = foldr (+) 0 $ take 4 [1, 2, 3, 4, undefined]        -- `10`

-- In the following, undefined is part of the spine.

_ = foldr (+) 0 $ [1, 2, 3, 4] ++ undefined             -- Throws an exception.
_ = foldr (+) 0 $ take 4 $ [1, 2, 3, 4] ++ undefined    -- `10`

-- -----------------------------------------------------------------------------

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs

_ = myAny even [1..]    -- `True`
{-
_ = myAny even $ repeat 1   -- Results in an infinite loop.
-}




-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
