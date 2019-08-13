module O26__Foldable () where

import Data.Maybe
import Data.Monoid
  ( Any(..)
  , All(..)
  , First(..)
  , Last(..)
  , Product(..)
  , Sum(..)
  )
import Data.Foldable (fold, toList)

-- `Foldable` gives us a way to process values embedded in a structure as if
-- they existed in a sequential order.

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

-- `foldMap` maps elements of a structure to a `Monoid` before folding it.

_ = foldMap Sum [1, 2, 3, 4]           -- `Sum { getSum = 10 }`
_ = foldMap Product [1, 2, 3, 4]       -- `Product { getProduct = 24 }`
_ = foldMap All [True, False, True]    -- `All { getAll = False }`
_ = foldMap Any [(3 == 4), (9 > 5)]    -- `Any { getAny = True }`

xs = [Just 1, Nothing, Just 5]
_ = foldMap First xs    -- `First { getFirst = Just 1 }`
_ = foldMap Last xs     -- `Last { getLast = Just 5 }`

-- -----------------------------------------------------------------------------

-- `foldMap` can also have a function to map that is different from the `Monoid`
-- it's using.

xs' = map Product [1..3]
_ = foldMap (*5) xs'    -- `Product {`getProduct = 750 }`
-- 5 * 10 * 15
-- 750

xs'' = map Sum [1..3]
_ = foldMap (*5) xs''    -- `Sum { getSum = 30 }`
-- 5 + 10 + 15
-- 30

-- -----------------------------------------------------------------------------

-- With `foldr` the `Monoid` instance is "baked-in".

_ = foldr (*) 5 [1, 2, 3]    -- `30`
-- (1 * (2 * (3 * 5)))

-- Due to the way `foldr` works, declaring a `Monoid` instance that is different
-- from what is implied in the folding function doesn't change the final result.

_ = foldr (*) 3 [2..4]    -- `72`

sumXs = map Sum [2..4]
_ = foldr (*) 3 sumXs        -- `Sum { getSum = 72 }`

productXs = map Product [2..4]
_ = foldr (*) 3 productXs    -- `Product { getProduct = 72 }`

-- -----------------------------------------------------------------------------

-- If what you're trying to fold only contains one value, declaring a `Monoid`
-- instance won't change the behavior of `foldMap`.

_ = foldMap (*5) (Just 100) :: Product Integer    -- `Product { getProduct = 500 }`
_ = foldMap (*5) (Just 5) :: Sum Integer          -- `Sum { getSum = 25 }`

-- With only one value, it doesn't need the `Monoid` instance. Specifying the
-- `Monoid` instance is necessary to satisfy the typechecker, but with only
-- one value, there is nothing to `mappend`. It just applies the function.
-- It will use the `mempty` value from the declared `Monoid` instance,
-- though, in cases where what you are trying to fold is empty.

_ = foldMap (*5) Nothing :: Sum Integer        -- `Sum { getSum = 0 }`
_ = foldMap (*5) Nothing :: Product Integer    -- `Product { getProduct = 1 }`

-- `Foldable` is a way of generalizing catamorphisms - folding - to different
-- datatypes, and at least in some cases, it forces you to think about the
-- monoid you're using to combine values.

-- -----------------------------------------------------------------------------

data Identity a = Identity a

-- We're only obligated to write `foldr` or `foldMap`, but we'll write both
-- plus `foldl` so you have the gist of it.

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

_ = foldr (*) 1 (Identity 5)    -- `5`
_ = foldl (*) 5 (Identity 5)    -- `25`

type PI = Product Integer

_ = foldMap (*5) (Identity 100) :: PI    -- `Product { getProduct = 500 }`

-- -----------------------------------------------------------------------------

-- For `foldr` and `foldl` the "zero" value is the start value given.

_ = foldr (+) 1 Nothing    -- `1`

-- For `foldMap` the "zero" value is the monoid's identity value.

_ = foldMap (+1) Nothing :: Sum Integer    -- `Sum { getSum = 0 }`

-- When the value is a `Just` value we need to apply the folding function to the
-- value and dispose of the `Maybe` structure.

_ = foldr (+) 1 (Just 3)                    -- `4`
_ = foldMap (+1) $ Just 3 :: Sum Integer    -- `Sum { getSum = 4 }`

-- -----------------------------------------------------------------------------

data Optional a = Nada | Yep a

instance Foldable Optional where
  foldr _ z Nada    = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada    = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

_ = foldMap (+1) Nada :: Sum Int        -- `Sum { getSum = 0 }`
_ = foldMap (+1) Nada :: Product Int    -- `Product { getProduct = 1 }`

{-
-- `foldMap` requires a type with a monoid.
_ = foldMap (+1) Nada    -- Error.
-}

_ = foldMap (+1) (Just 1) :: Sum Int    -- `Sum { getSum = 2 }`

-- -----------------------------------------------------------------------------

-- `toList` lists elements of a structure from left to right.
--
-- toList :: Foldable t => t a -> [a]

_ = toList $ Just 1                               -- `[1]`
_ = toList (1, 2)                                 -- `[2]`
_ = map toList [Just 1, Just 2, Just 3]           -- `[[1], [2], [3]]`
_ = concatMap toList [Just 1, Just 2, Just 3]     -- `[1, 2, 3]`
_ = concatMap toList [Just 1, Just 2, Nothing]    -- `[1, 2]`

-- -----------------------------------------------------------------------------

-- `null` checks whether a structure is empty.
--
-- null :: Foldable t => t a -> Bool

_ = null $ Left 3                          -- `True`
_ = null []                                -- `True`
_ = null Nothing                           -- `True`
_ = null (1, 2)                            -- `False`
_ = fmap null [Just 1, Just 2, Nothing]    -- `[False, False, True]`
_ = null <$> [Just 1, Just 2, Nothing]     -- `[False, False, True]`

-- -----------------------------------------------------------------------------

-- `length` counts how many values inhabit the `Foldable`.
--
-- length :: Foldable t => t a -> Int

_ = length (1, 2)                            -- `1`
_ = length [(1, 2), (3, 4), (5, 6)]          -- `3`
_ = length <$> [(1, 2), (3, 4), (5, 6)]      -- `[1, 1, 1]`
_ = length <$> Just [1, 2, 3]                -- `Just 3`
_ = length <$> Just $ [1, 2, 3]              -- `1`
_ = fmap length $ Just [1, 2, 3]             -- `Just 3`
_ = fmap length Just $ [1, 2, 3]             -- `1`
_ = fmap length Just [1, 2, 3]               -- `1`
_ = fmap length [Just 1, Just 2, Just 3]     -- `[1, 1, 1]`
_ = fmap length [Just 1, Just 2, Nothing]    -- `[1, 1, 0]`

-- -----------------------------------------------------------------------------

-- `elem` checks if an element is in the structure.
--
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool

_ = elem 2 (Just 3)                              -- `False`
_ = elem True (Left False)                       -- `False`
_ = elem True (Left True)                        -- `False`
_ = elem True (Right False)                      -- `False`
_ = elem True (Right True)                       -- `True`
_ = fmap (elem 3) [Right 1, Right 2, Right 3]    -- `[False, False, True]`
_ = elem 3 <$> [Right 1, Right 2, Right 3]       -- `[False, False, True]`

-- -----------------------------------------------------------------------------

-- `maximum` gets the largest element of a non-empty structure.
--
-- maximum :: (Foldable t, Ord a) => t a -> a

_ = maximum [10, 12, 33, 5]                  -- `33`
_ = maximum <$> [Just 2, Just 10, Just 4]    -- `[2, 10, 4]`
_ = fmap maximum (Just [3, 7, 10, 2])        -- `Just 10`
_ = maximum <$> Just [3, 7, 10, 2]           -- `Just 10`

-- `minimum` gets the smallest element of a non-empty structure.
--
-- minimum :: (Foldable t, Ord a) => t a -> a

_ = minimum "julie"                    -- `'e'`
_ = minimum <$> Just "julie"           -- `Just 'e'`
_ = minimum <$> map Just "julie"       -- "julie"
_ = map Just "julie"                   -- `[Just 'j', Just 'u', Just 'l', Just 'i', Just 'e']`
_ = fmap minimum Just "julie"          -- `"julie"`
_ = fmap minimum $ map Just "julie"    -- `"julie"`
_ = fmap minimum (Just "julie")        -- `Just 'e'`
_ = fmap minimum ["julie"]             -- `"e"`

{-
-- The structure must be non-empty.
_ = fmap minimum [Just 4, Just 3, Nothing]    -- Exception.
-}

-- -----------------------------------------------------------------------------

_ = sum (7, 5)                         -- `5`
_ = fmap sum [(7, 5), (3, 4)]          -- `[5, 4]`
_ = fmap sum (Just [1, 2, 3, 4, 5])    -- `Just 15`

_ = product Nothing                    -- `1`
_ = fmap product (Just [])             -- `Just 1`
_ = fmap product (Right [1, 2, 3])     -- `Right 6`

-- -----------------------------------------------------------------------------

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = foldr (+) 0

_ = sum' (7, 5)                         -- `5`
_ = fmap sum' [(7, 5), (3, 4)]          -- `[5, 4]`
_ = fmap sum' (Just [1, 2, 3, 4, 5])    -- `Just 15`

_ = sum'' (7, 5)                         -- `5`
_ = fmap sum'' [(7, 5), (3, 4)]          -- `[5, 4]`
_ = fmap sum'' (Just [1, 2, 3, 4, 5])    -- `Just 15`

-- -----------------------------------------------------------------------------

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

product'' :: (Foldable t, Num a) => t a -> a
product'' = foldr (*) 1

_ = product' Nothing                   -- `1`
_ = fmap product' (Just [])            -- `Just 1`
_ = fmap product' (Right [1, 2, 3])    -- `Right 6`

_ = product'' Nothing                   -- `1`
_ = fmap product'' (Just [])            -- `Just 1`
_ = fmap product'' (Right [1, 2, 3])    -- `Right 6`

-- -----------------------------------------------------------------------------

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a ta = getAny $ foldMap (\x -> Any $ x == a) ta

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' a ta = foldr (\x y -> x == a || y) False ta

_ = elem' 2 (Just 3)                           -- `False`
_ = elem' True (Right True)                    -- `True`
_ = elem' 3 <$> [Right 1, Right 2, Right 3]    -- `[False, False, True]`

_ = elem'' 2 (Just 3)                           -- `False`
_ = elem'' True (Right True)                    -- `True`
_ = elem'' 3 <$> [Right 1, Right 2, Right 3]    -- `[False, False, True]`

-- -----------------------------------------------------------------------------

data Min' a = MinNothing | Min' a deriving (Eq)

instance Ord a => Semigroup (Min' a) where
  (<>) MinNothing a          = a
  (<>) a          MinNothing = a
  (<>) (Min' a)   (Min' b)   = Min' $ min a b

instance Ord a => Monoid (Min' a) where
  mempty = MinNothing

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' ta =
  case foldMap Min' ta of
    MinNothing -> Nothing
    Min' a     -> Just a

minimum'' :: (Foldable t, Ord a) => t a -> Maybe a
minimum'' = foldr (\x y -> min (Just x) y) Nothing

_ = minimum' "julie"                    -- `Just 'e'`
_ = minimum' <$> map Just "julie"       -- `[Just 'j', Just 'u', Just 'l', Just 'i', Just 'e']`
_ = fmap minimum' ["julie"]             -- `[Just 'e']`

_ = minimum'' "julie"                    -- `Just 'e'`
_ = minimum'' <$> map Just "julie"       -- `[Just 'j', Just 'u', Just 'l', Just 'i', Just 'e']`
_ = fmap minimum'' ["julie"]             -- `[Just 'e']`

-- -----------------------------------------------------------------------------

data Max' a = MaxNothing | Max' a deriving (Eq)

instance Ord a => Semigroup (Max' a) where
  (<>) MaxNothing a          = a
  (<>) a          MaxNothing = a
  (<>) (Max' a)   (Max' b)   = Max' $ max a b

instance Ord a => Monoid (Max' a) where
  mempty = MaxNothing

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' ta =
  case foldMap Max' ta of
    MaxNothing -> Nothing
    Max' a     -> Just a

maximum'' :: (Foldable t, Ord a) => t a -> Maybe a
maximum'' = foldr (\x y -> max (Just x) y) Nothing

_ = maximum' [10, 12, 33, 5]                  -- `Just 33`
_ = maximum' <$> [Just 2, Just 10, Just 4]    -- `[Just 2, Just 10, Just 4]`
_ = maximum' <$> Just [3, 7, 10, 2]           -- `Just (Just 10)`

_ = maximum'' [10, 12, 33, 5]                  -- `Just 33`
_ = maximum'' <$> [Just 2, Just 10, Just 4]    -- `[Just 2, Just 10, Just 4]`
_ = maximum'' <$> Just [3, 7, 10, 2]           -- `Just (Just 10)`

-- -----------------------------------------------------------------------------

null' :: Foldable t => t a -> Bool
null' ta = (not . getAny) $ foldMap (\_ -> Any $ length ta /= 0) ta

null'' :: Foldable t => t a -> Bool
null'' = foldr (\_ _ -> False) True

_ = null' []                               -- `True`
_ = null' Nothing                          -- `True`
_ = null' <$> [Just 1, Just 2, Nothing]    -- `[False, False, True]`

_ = null'' []                               -- `True`
_ = null'' Nothing                          -- `True`
_ = null'' <$> [Just 1, Just 2, Nothing]    -- `[False, False, True]`

-- -----------------------------------------------------------------------------

length' :: Foldable t => t a -> Int
length' = getSum . foldMap (\_ -> Sum 1)

length'' :: Foldable t => t a -> Int
length'' = foldr (\_ n -> n + 1) 0

_ = length' (1, 2)                            -- `1`
_ = length' [(1, 2), (3, 4), (5, 6)]          -- `3`
_ = length' <$> [(1, 2), (3, 4), (5, 6)]      -- `[1, 1, 1]`
_ = length' <$> Just [1, 2, 3]                -- `Just 3`
_ = fmap length' [Just 1, Just 2, Nothing]    -- `[1, 1, 0]`

_ = length'' (1, 2)                            -- `1`
_ = length'' [(1, 2), (3, 4), (5, 6)]          -- `3`
_ = length'' <$> [(1, 2), (3, 4), (5, 6)]      -- `[1, 1, 1]`
_ = length'' <$> Just [1, 2, 3]                -- `Just 3`
_ = fmap length'' [Just 1, Just 2, Nothing]    -- `[1, 1, 0]`

-- -----------------------------------------------------------------------------

toList' :: Foldable t => t a -> [a]
toList' = foldMap (\x -> [x])

toList'' :: Foldable t => t a -> [a]
toList'' = foldr (:) []

_ = toList' Nothing     -- `Nothing`
_ = toList' $ Just 2    -- `[2]`

_ = toList'' Nothing     -- `Nothing`
_ = toList'' $ Just 2    -- `[2]`

-- -----------------------------------------------------------------------------

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap $ (<>) mempty

fold'' :: (Foldable t, Monoid m) => t m -> m
fold'' = foldr (<>) mempty

-- -----------------------------------------------------------------------------

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f ta = foldr (\x acc -> f x <> acc) mempty ta

-- -----------------------------------------------------------------------------

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

-- -----------------------------------------------------------------------------

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

-- -----------------------------------------------------------------------------

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- -----------------------------------------------------------------------------

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

-- -----------------------------------------------------------------------------

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

-- -----------------------------------------------------------------------------

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f ta = foldMap (\a -> if f a then pure a else mempty) ta

_ = filterF even [1, 2, 3, 4] :: [Int]                          -- `[2, 4]`
_ = filterF isJust [Nothing, Just 1, Nothing] :: [Maybe Int]    -- `[Just 1]`
