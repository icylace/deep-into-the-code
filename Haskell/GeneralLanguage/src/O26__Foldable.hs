module O26__Foldable where

import Data.Monoid (Sum(..))

import Data.Foldable (fold)

-- While `fold` allows you to combine elements inside a `Foldable` structure
-- using the `Monoid` defined for those elements, `foldMap` first maps
-- each element of the structure to a `Monoid` and then combines the
-- results using that instance of `Monoid`.

_ = foldr (+) 0 [1..5]                         -- `15`
_ = fold ([1, 2, 3, 4, 5] :: [Sum Integer])    -- `Sum { getSum = 15 }`
_ = fold $ map Sum [1..5]                      -- `Sum { getSum = 15 }`

_ = fold ([1, 2, 3, 4, 5] :: [Product Integer])    -- `Product { getProduct = 120 }`

-- -----------------------------------------------------------------------------

-- In some cases, the compiler can identify and use the standard `Monoid` for a
-- type, without us being explicit.

_ = concat ["hello", " julie"]           -- `"hello julie"`
_ = foldr (++) "" ["hello", " julie"]    -- `"hello julie"`
_ = foldr (<>) "" ["hello", " julie"]    -- `"hello julie"`
_ = fold ["hello", " julie"]             -- `"hello julie"`

-- -----------------------------------------------------------------------------

