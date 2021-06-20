module O24__Applicative () where

import Control.Applicative (ZipList, liftA2, liftA3)
import Data.Char (toUpper)
import Data.Functor.Identity (Identity(..))
import Data.List (elemIndex)
import Data.Map qualified as M (fromList, lookup)
import Data.Monoid (All(..), Product(..), Sum(..))

import Test.QuickCheck.Checkers (EqProp, (=-=), eq)

-- Applicatives are monoidal functors.

-- `Monoid` gives us a means of mashing two values of the same type together.
-- `Monoid`'s core operation, `mappend`, smashes the structures together -
-- when you mappend two lists, they become one list, so the structures
-- themselves have been joined.

-- `Functor` is for function application over some structure we don't want to
-- have to think about. The core operation of `Functor`, `fmap`, applies a
-- function to a value that is within some structure while leaving that
-- structure unaltered.

-- `Applicative` allows for function application lifted over structure (like
-- Functor). But with `Applicative` the function we're applying is also
-- embedded in some structure. Because the function and the value it's
-- being applied to both have structure, we have to smash those
-- structures together. So, `Applicative` involves monoids
-- and functors.

-- Basically, an applicative is just a way of saying we mapped a function over
-- some functorial `f` or it was already in `f` somehow and we hit a situation
-- where we want to map `f (a -> b)` not just `(a -> b)` over some `f a`
-- to get an `f b`.

-- This is only here for reference.
class Functor f => Applicative' f where
  pure' :: a -> f a
  (<*>.) :: f (a -> b) -> f a -> f b

-- The application operator, `$`, map operator, `<$>`, from `Functor`, and the
-- apply operator (a.k.a. "ap"), `<*>`, from `Applicative` are very similar.
--
--  ($)  ::                    (a -> b) ->   a ->   b
-- (<$>) ::     Functor f =>   (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- From the `Control.Applicative` module:
--
-- liftA  :: Applicative f => (a -> b)           -> f a -> f b
-- liftA2 :: Applicative f => (a -> b -> c)      -> f a -> f b -> f c
-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

-- -----------------------------------------------------------------------------

-- `pure` wraps a value within a structure.

_ = pure 1 :: [Int]           -- `[1]`
_ = pure 1 :: Maybe Int       -- `Just 1`
_ = pure 1 :: Either a Int    -- `Right 1`
_ = pure 1 :: ([a], Int)      -- `([], 1)`

add :: Maybe (Int -> Int -> Int)
add = pure (+)

_ = add <*> (Just 5) <*> (Just 8)         -- `Just 13`
_ = pure (+) <*> (Just 5) <*> (Just 8)    -- `Just 13`
_ = liftA2 (+) (Just 5) (Just 8)          -- `Just 13`

add' = pure (+) :: Maybe (Int -> Int -> Int)

_ = add' <*> (Just 5) <*> (Just 8)    -- `Just 13`

-- -----------------------------------------------------------------------------

-- A functor can be defined in terms of an applicative.
--
-- fmap f x = pure f <*> x

_ = fmap (+1) [1, 2, 3]     -- `[2, 3, 4]`
_ = pure (+1) <*> [1..3]    -- `[2, 3, 4]`

-- -----------------------------------------------------------------------------

-- The left type is handled differently from the right in the final two examples
-- for the same reason as here:

_ = fmap (+1) (4, 5)    -- `(4, 6)`

-- The left type is part of the structure, and the structure is not transformed
-- by the function application.

-- With `Applicative` we have a `Monoid` for our structure and function
-- application for our values.
--
-- mappend :: f          -> f   -> f
-- $       ::   (a -> b) ->   a ->   b
-- (<*>)   :: f (a -> b) -> f a -> f b

_ = [(+1), (*2)] <*> [2, 4]    -- `[3, 5, 4, 8]`
-- `[(+1) 2, (+1) 4, (*2) 2, (*2) 4]`
-- `[3, 5, 4, 8]`

_ = Just (*2) <*> Just 2     -- `Just 4`
_ = Just (*2) <*> Nothing    -- `Nothing`
_ = Nothing <*> Just 2       -- `Nothing`
_ = Nothing <*> Nothing      -- `Nothing`

_ = ("Woo", (+1)) <*> (" Hoo!", 0)          -- `("Woo Hoo!", 1)`
_ = (Sum 2, (+1)) <*> (Sum 1, 0)            -- `(Sum { getSum = 3 }, 1)`
_ = (Product 3, (+9)) <*> (Product 2, 8)    -- `(Product { getProduct = 6 }, 17)`
_ = (All True, (+1)) <*> (All False, 0)     -- `(All { getAll = False }, 1)`

-- -----------------------------------------------------------------------------

-- We can map a multiparameter function into a structure for partial application
-- and then apply that to a value of the same structure for further application.

_ = (,) <$> [1, 2] <*> [3, 4]    -- `[(1, 3), (1, 4), (2, 3), (2, 4)]`
-- `[(,) 1, (,) 2] <*> [3, 4]`
-- `[(,) 1 3, (,) 1 4, (,) 2 3, (,) 2 4]`
-- `[(1, 3), (1, 4), (2, 3), (2, 4)]`

_ = liftA2 (,) [1, 2] [3, 4]    -- `[(1, 3), (1, 4), (2, 3), (2, 4)]`

_ = (+) <$> [1, 2] <*> [3, 5]    -- `[4, 6, 5, 7]`
_ = liftA2 (+) [1, 2] [3, 5]     -- `[4, 6, 5, 7]`

_ = max <$> [1, 2] <*> [1, 4]    -- `[1, 4, 2, 4]`
_ = liftA2 max [1, 2] [1, 4]     -- `[1, 4, 2, 4]`

-- -----------------------------------------------------------------------------

u (x:xs) = toUpper x:xs

l = lookup 3 [(3, "hello")]    -- `Just "hello"`
_ = fmap length l              -- `Just 5`
_ = fmap u l                   -- `Just "Hello"`

_ = fmap u $ M.lookup 3 $ M.fromList [(3, "hello")]    -- `Just "Hello"`

-- -----------------------------------------------------------------------------

f x = lookup x [ (3, "hello")
               , (4, "julie")
               , (5, "kbai")
               ]

g y = lookup y [ (7, "sup?")
               , (8, "chris")
               , (9, "aloha")
               ]

h z = lookup z [(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

_ = f 3                     -- `Just "hello"`
_ = g 8                     -- `Just "chris"`
_ = (++) <$> f 3 <*> g 7    -- `Just "hellosup?"`
_ = (+) <$> h 5 <*> m 1     -- `Just 9007`
_ = (+) <$> h 5 <*> m 6     -- `Nothing`

_ = liftA2 (++) (g 9) (f 4)    -- `Just "alohajulie"`
_ = liftA2 (^) (h 5) (m 4)     -- `Just 60466176`
_ = liftA2 (*) (h 5) (m 4)     -- `Just 60`
_ = liftA2 (*) (h 1) (m 1)     -- `Nothing`

{- GHCi ------------------------------------------------------------------------

> (++) <$> getLine <*> getLine
abc
def
"abcdef"

> (,) <$> getLine <*> getLine
abc
def
("abc","def")

> fmap length $ (++) <$> getLine <*> getLine
abc
def
6

> length <$> ((++) <$> getLine <*> getLine)
abc
def
6

-------------------------------------------------------------------------------}

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])    -- `Just 9`

-- -----------------------------------------------------------------------------

a :: Maybe Integer
a = lookup 3 $ zip [1, 2, 3] [4, 5, 6]    -- `Just 6`

b :: Maybe Integer
b = lookup 2 $ zip [1, 2, 3] [4, 5, 6]    -- `Just 5`

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> a <*> b    -- `Just (6, 5)`

-- -- -----------------------------------------------------------------------------

c :: Maybe Int
c = elemIndex 3 [1, 2, 3, 4, 5]    -- `Just 2`

d :: Maybe Int
d = elemIndex 4 [1, 2, 3, 4, 5]    -- `Just 3`

-- TODO:
-- describe type specificity better
--
-- > :t max
-- max :: Ord a => a -> a -> a
max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> c <*> d    -- `Just 3`

-- -----------------------------------------------------------------------------

xs = [1, 2, 3]
ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys    -- `Just 6`

y :: Maybe Integer
y = lookup 2 $ zip xs ys     -- `Just 5`

summed :: Maybe Integer
summed = sum <$> ((,) <$> x <*> y)    -- `Just 5`

summed' :: Maybe Integer
summed' = fmap sum $ (,) <$> x <*> y    -- `Just 5`

-- -----------------------------------------------------------------------------

-- The `Identity` type is a way to introduce structure without changing the
-- semantics of what you're doing.

zs = [1, 2, 3]
zs' = [9, 9, 9]

zz :: [Integer]
zz = const <$> zs <*> zs'    -- `[1, 1, 1, 2, 2, 2, 3, 3, 3]`

zz' :: Identity [Integer]
zz' = const <$> Identity zs <*> Identity zs'    -- `Identity [1, 2, 3]`

-- When `Identity` was used above, the mapping of `const` was shifted to
-- affecting it instead of the structure it contained thereby leaving
-- the original structure untouched.

-- -----------------------------------------------------------------------------

newtype Identity' a = Identity' a
  deriving (Eq, Ord, Show)

instance Functor Identity' where
  fmap f (Identity' x) = Identity' (f x)

instance Applicative Identity' where
  pure = Identity'
  (<*>) (Identity' f) (Identity' x) = Identity' (f x)

-- -----------------------------------------------------------------------------

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant x) (Constant y) = Constant (x <> y)

ff = Constant (Sum 1)
gg = Constant (Sum 2)
_ = ff <*> gg    -- Constant { getConstant = Sum { getSum = 3 } }

_ = pure 1 :: Constant String Int    -- `Constant { getConstant = "" }`

-- -----------------------------------------------------------------------------

-- This is only here for reference.
data Maybe' a = Nothing' | Just' a

-- This is only here for reference.
instance Functor Maybe' where
  fmap _ Nothing' = Nothing'
  fmap f (Just' a) = Just' (f a)

-- This is only here for reference.
instance Applicative Maybe' where
  pure = Just'

  -- Nothing' <*> _        = Nothing'
  -- _        <*> Nothing' = Nothing'
  -- Just' f  <*> Just' x  = Just' (f x)

  Just' f  <*> Just' x  = Just' (f x)
  _        <*> _        = Nothing'

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' -> Just $ Person n' a'

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a

addy = mkAddress "old macdonald's"
person = fmap Person $ mkName "Babe"

_ = person <*> addy                      -- `Just (Person (Name "Babe") (Address "old macdonald's"))`
_ = Person <$> mkName "Babe" <*> addy    -- `Just (Person (Name "Babe") (Address "old macdonald's"))`
_ = mkPerson "Babe" "old macdonald's"    -- `Just (Person (Name "Babe") (Address "old macdonald's"))`

-- -----------------------------------------------------------------------------

data Cow = Cow
  { name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty x  = Just x

noNegative :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              Just (Cow nammy agey weighty)

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
  Cow <$> noEmpty name'
      <*> noNegative age'
      <*> noNegative weight'

cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' name' age' weight' =
  liftA3 Cow (noEmpty name')
             (noNegative age')
             (noNegative weight')

cow1 :: Maybe (Int -> Int -> Cow)
cow1 = Cow <$> noEmpty "Bess"

cow1'' :: Maybe (Int -> Int -> Cow)
cow1'' = fmap Cow (noEmpty "Bess")

cow1' :: Applicative f => f String -> f Int -> f Int -> f Cow
cow1' = liftA3 Cow

cow2 :: Maybe (Int -> Cow)
cow2 = cow1 <*> noNegative 1

cow2' :: Maybe Int -> Maybe Int -> Maybe Cow
cow2' = cow1' (noEmpty "blah")

cow3 :: Maybe Cow
cow3 = cow2 <*> noNegative 2

cow3' :: Maybe Int -> Maybe Cow
cow3' = cow2' (noNegative 1)

cow4' :: Maybe Cow
cow4' = cow3' (noNegative 2)

maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeApply = (<*>)

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap = fmap

-- -----------------------------------------------------------------------------

_ = const <$> Just "Hello" <*> pure "World"    -- `Just "Hello"`

_ = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]    -- `Just (90, 10, "Tierness", [1,2,3])`

-- -----------------------------------------------------------------------------

-- Applicative laws:

-- -----------------------------------------------------------------------------

-- Identity:
-- pure id <*> v = v

_ = pure id <*> [1..5]                      -- `[1, 2, 3, 4, 5]`
_ = pure id <*> Just "Hello Applicative"    -- `Just "Hello Applicative"`
_ = pure id <*> Nothing                     -- `Nothing`
_ = pure id <*> Left "Error'ish"            -- `Left "Error'ish"`
_ = pure id <*> Right 8001                  -- `Right 8001`
_ = pure id <*> (+1) $ 2                    -- `3`

_ =      id  $  [1..5]    -- `[1, 2, 3, 4, 5]`
_ =      id <$> [1..5]    -- `[1, 2, 3, 4, 5]`
_ = pure id <*> [1..5]    -- `[1, 2, 3, 4, 5]`

-- -----------------------------------------------------------------------------

-- Composition:
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

_ = pure (.) <*> [(+1)] <*> [(*2)] <*> [1, 2, 3]    -- `[3, 5, 7]`
_ = [(+1)] <*> ([(*2)] <*> [1, 2, 3])               -- `[3, 5, 7]`

_ = pure (.) <*> Just (+1) <*> Just (*2) <*> Just 1    -- `Just 3`
_ = Just (+1) <*> (Just (*2) <*> Just 1)               -- `Just 3`

-- -----------------------------------------------------------------------------

-- Homomorphism:
-- pure f <*> pure x = pure (f x)

-- Homomorphism: A structure-preserving map between two algebraic structures.

-- The effect of applying a function that is embedded in some structure to a
-- value that is embedded in some structure should be the same as applying a
-- function to a value without affecting any outside structure.

-- The general idea of the homomorphism law is that applying the function
-- doesn't change the structure around the values.

_ = Just $ (+1) 1                        -- `Just 2`
_ = pure (+1) <*> pure 1 :: Maybe Int    -- `Just 2`
_ = pure ((+1) 1) :: Maybe Int           -- `Just 2`

_ = [(+1) 1]                         -- `[2]`
_ = pure (+1) <*> pure 1 :: [Int]    -- `[2]`
_ = pure ((+1) 1) :: [Int]           -- `[2]`

_ = Right $ (+1) 1                          -- `Right 2`
_ = pure (+1) <*> pure 1 :: Either a Int    -- `Right 2`
_ = pure ((+1) 1) :: Either a Int           -- `Right 2`

-- -----------------------------------------------------------------------------

-- Interchange:
-- u <*> pure y = pure ($ y) <*> u

_ = Just (+2) <*> pure 2        -- `Just 4`
_ = pure ($ 2) <*> Just (+2)    -- `Just 4`

_ = [(+1), (*2)] <*> pure 1        -- `[2, 2]`
_ = pure ($ 1) <*> [(+1), (*2)]    -- `[2, 2]`

_ = Just (+3) <*> pure 1           -- `Just 4`
_ = pure ($ 1) <*> Just (+3)       -- `Just 4`

-- -----------------------------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x ys) = Cons (f x) (fmap f ys)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure x = Cons x Nil

  (<*>) Nil  _   = Nil
  (<*>) _    Nil = Nil
  (<*>) (Cons f fs) xs = append (f <$> xs) (fs <*> xs)

v = Cons (+1) (Cons (*2) Nil)
v' = Cons 1 (Cons 2 Nil)
_ = v <*> v'    -- Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

_ = fmap (\x -> [x, 9]) [1, 2, 3]    -- `[[1, 9], [2, 9], [3, 9]]`

toMyList = foldr Cons Nil
xs2 = toMyList [1, 2, 3]

f22 x = x `Cons` (9 `Cons` Nil)
_ = flatMap f22 xs2    -- `Cons 1 (Cons 9 (Cons 2 (Cons 9 (Cons 3 (Cons 9 Nil)))))`

-- -----------------------------------------------------------------------------

-- `Applicative` can have more than one valid and lawful instance for a
-- given datatype.

-- -----------------------------------------------------------------------------

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take 3000 l
          ys' = let (ZipList' l) = ys in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' [x]

  (<*>) (ZipList' [])  _              = ZipList' []
  (<*>) _              (ZipList' [])  = ZipList' []
  (<*>) (ZipList' [f]) (ZipList' xs)  = ZipList' $ map f xs
  (<*>) (ZipList' fs)  (ZipList' [x]) = ZipList' $ map ($ x) fs
  (<*>) (ZipList' fs)  (ZipList' xs)  = ZipList' $ zipWith ($) fs xs

az = ZipList' [(+9), (*2), (+8)]
_ = az <*> ZipList' [1..3]         -- ZipList' [10, 4, 11]
_ = az <*> pure 1                  -- ZipList' [10, 2, 9]
_ = pure id <*> ZipList' [1, 2]    -- ZipList' [1, 2]

-- -----------------------------------------------------------------------------

_ = pure 1 :: Either e Int              -- `Right 1`
_ = Right (+1) <*> Right 1              -- `Right 2`
_ = Right (+1) <*> Left ":("            -- `Left ":("`
_ = Left ":(" <*> Right 1               -- `Left ":("`
_ = Left ":(" <*> Left "sadface.png"    -- `Left ":("`

-- -----------------------------------------------------------------------------

data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success

  (<*>) (Failure e) (Success _) = Failure e
  (<*>) (Success _) (Failure e) = Failure e
  (<*>) (Failure d) (Failure e) = Failure (d <> e)
  (<*>) (Success f) (Success a) = Success (f a)

-- This function is a natural transformation.
validationToEither :: Validation e a -> Either e a
validationToEither (Failure e) = Left e
validationToEither (Success a) = Right a

-- This function is a natural transformation.
eitherToValidation :: Either e a -> Validation e a
eitherToValidation (Left e)  = Failure e
eitherToValidation (Right a) = Success a

data Errors = DividedByZero | StackOverflow | MooglesChewedWires deriving (Eq, Show)

success :: Validation [Errors] Int
success = Success (+1) <*> Success 1    -- `Success 2`

failure :: Validation [Errors] Int
failure = Success (+1) <*> Failure [StackOverflow]    -- `Failure [StackOverflow]`

failure' :: Validation [Errors] Int
failure' = Failure [StackOverflow] <*> Success (+1)    -- `Failure [StackOverflow]`

failures :: Validation [Errors] Int
failures = Failure [MooglesChewedWires] <*> Failure [StackOverflow]    -- `Failure [MooglesChewedWires, StackOverflow]`

-- -----------------------------------------------------------------------------

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

_ = combos stops vowels stops    -- `[('p','a','p'),('p','a','b'),('p','a','t'),('p','a','d'),('p','a','k'),('p','a','g'),('p','e','p'),('p','e','b'),('p','e','t'),('p','e','d'),('p','e','k'),('p','e','g'),('p','i','p'),('p','i','b'),('p','i','t'),('p','i','d'),('p','i','k'),('p','i','g'),('p','o','p'),('p','o','b'),('p','o','t'),('p','o','d'),('p','o','k'),('p','o','g'),('p','u','p'),('p','u','b'),('p','u','t'),('p','u','d'),('p','u','k'),('p','u','g'),('b','a','p'),('b','a','b'),('b','a','t'),('b','a','d'),('b','a','k'),('b','a','g'),('b','e','p'),('b','e','b'),('b','e','t'),('b','e','d'),('b','e','k'),('b','e','g'),('b','i','p'),('b','i','b'),('b','i','t'),('b','i','d'),('b','i','k'),('b','i','g'),('b','o','p'),('b','o','b'),('b','o','t'),('b','o','d'),('b','o','k'),('b','o','g'),('b','u','p'),('b','u','b'),('b','u','t'),('b','u','d'),('b','u','k'),('b','u','g'),('t','a','p'),('t','a','b'),('t','a','t'),('t','a','d'),('t','a','k'),('t','a','g'),('t','e','p'),('t','e','b'),('t','e','t'),('t','e','d'),('t','e','k'),('t','e','g'),('t','i','p'),('t','i','b'),('t','i','t'),('t','i','d'),('t','i','k'),('t','i','g'),('t','o','p'),('t','o','b'),('t','o','t'),('t','o','d'),('t','o','k'),('t','o','g'),('t','u','p'),('t','u','b'),('t','u','t'),('t','u','d'),('t','u','k'),('t','u','g'),('d','a','p'),('d','a','b'),('d','a','t'),('d','a','d'),('d','a','k'),('d','a','g'),('d','e','p'),('d','e','b'),('d','e','t'),('d','e','d'),('d','e','k'),('d','e','g'),('d','i','p'),('d','i','b'),('d','i','t'),('d','i','d'),('d','i','k'),('d','i','g'),('d','o','p'),('d','o','b'),('d','o','t'),('d','o','d'),('d','o','k'),('d','o','g'),('d','u','p'),('d','u','b'),('d','u','t'),('d','u','d'),('d','u','k'),('d','u','g'),('k','a','p'),('k','a','b'),('k','a','t'),('k','a','d'),('k','a','k'),('k','a','g'),('k','e','p'),('k','e','b'),('k','e','t'),('k','e','d'),('k','e','k'),('k','e','g'),('k','i','p'),('k','i','b'),('k','i','t'),('k','i','d'),('k','i','k'),('k','i','g'),('k','o','p'),('k','o','b'),('k','o','t'),('k','o','d'),('k','o','k'),('k','o','g'),('k','u','p'),('k','u','b'),('k','u','t'),('k','u','d'),('k','u','k'),('k','u','g'),('g','a','p'),('g','a','b'),('g','a','t'),('g','a','d'),('g','a','k'),('g','a','g'),('g','e','p'),('g','e','b'),('g','e','t'),('g','e','d'),('g','e','k'),('g','e','g'),('g','i','p'),('g','i','b'),('g','i','t'),('g','i','d'),('g','i','k'),('g','i','g'),('g','o','p'),('g','o','b'),('g','o','t'),('g','o','d'),('g','o','k'),('g','o','g'),('g','u','p'),('g','u','b'),('g','u','t'),('g','u','d'),('g','u','k'),('g','u','g')]`

-- -----------------------------------------------------------------------------

-- (<$)  ::     Functor f =>   a -> f b -> f a
-- (<*)  :: Applicative f => f a -> f b -> f a
-- const ::                    a ->   b ->   a

-- -----------------------------------------------------------------------------

_ = (,) [1] ['a']         -- `([1], "a")`
_ = liftA2 (,) [1] [2]    -- `[(1, 'a')]`

_ = True || False          -- `True`
_ = False || False         -- `False`
_ = (2 > 3) || (3 == 3)    -- `True`

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)

fff 9001 = True ; fff _ = False
ggg 42 = True ; ggg _ = False

_ = fff 42      -- `False`
_ = fff 9001    -- `True`
_ = ggg 42      -- `True`
_ = ggg 9001    -- `False`

_ = (\n -> fff n || ggg n) 0       -- `False`
_ = (\n -> fff n || ggg n) 9001    -- `True`

_ = (fff <||> ggg) 0       -- `False`
_ = (fff <||> ggg) 9001    -- `True`

-- It's parallel application of the functions against an argument. That
-- application produces two values, so we monoidally combine the two
-- values so that we have a single value to return. We've set up an
--  environment so that two `(a -> Bool)` functions that don't have
-- an `a` argument yet can return a result based on those two
-- `Bool` values when the combined function is eventually
-- applied against an `a`.
