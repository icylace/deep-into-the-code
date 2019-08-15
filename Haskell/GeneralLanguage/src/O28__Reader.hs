{-# LANGUAGE InstanceSigs #-}

module O28__Reader () where

import Control.Applicative (liftA2)
import Control.Monad
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

bbop' :: Integer -> Integer
bbop' x = boop x + doop x

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- When `boopDoop` receives an input, it will fill the empty slots in `boop` and
-- `doop`. The results will be bound to the variables `a` and `b` and passed
-- into `return`.
boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return $ a + b

_ = boop 3        -- `6`
_ = doop 3        -- `13`
_ = bip 3         -- `26`
_ = bloop 3       -- `26`
_ = bbop 3        -- `19`
_ = bbop' 3       -- `19`
_ = duwop 3       -- `19`
_ = boopDoop 3    -- `19`

-- -----------------------------------------------------------------------------

-- Mapping a function awaiting two arguments over a function awaiting one
-- produces a two argument function. This is identical to
-- function composition.

_ = ((+) . (*2)) 5 3           -- `13`
_ = ((+) <$> (*2)) 5 3         -- `13`
_ = (\x -> (+) (2 * x)) 5 3    -- `13`

_ = ((+) <$> (*2) <*> (+10)) 3        -- `19`
_ = (\x -> (+) (x * 2) (x + 10)) 3    -- `19`
_ = (\x -> (x * 2) + (x + 10)) 3      -- `19`
_ = (3 * 2) + (3 + 10)                -- `19`

-- -----------------------------------------------------------------------------

-- The `Functor` of functions is function composition.

-- The `Applicative` and `Monad` chain the argument forward in addition to the
-- composition (applicatives and monads are both varieties of functors,
-- so they retain that core functorial behavior).

-- -----------------------------------------------------------------------------

-- `Reader` is a way of stringing functions together when all those functions
-- are awaiting one input from a shared environment.  It's another way of
-- abstracting out function application and lets us do computation in
-- terms of an argument that hasn't been supplied yet. We use this
-- most often when we have a constant value that we will obtain
-- from somewhere outside our program that will be an argument
-- to a whole bunch of functions. Using `Reader` allows us to
-- avoid passing that argument around explicitly.

-- -----------------------------------------------------------------------------

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

_ = composed "Julie"    -- `"EILUJ"`
_ = fmapped "Chris"     -- `"SIRHC"`

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = do
  a <- cap
  b <- rev
  return (a, b)

tupled''' :: [Char] -> ([Char], [Char])
tupled''' = cap >>= \a ->
            rev >>= \b ->
            return (a, b)

_ = tupled "Julie"       -- `("JULIE", "eiluJ")`
_ = tupled' "Julie"      -- `("eiluJ", "JULIE")`
_ = tupled'' "Julie"     -- `("JULIE", "eiluJ")`
_ = tupled''' "Julie"    -- `("JULIE", "eiluJ")`

-- -----------------------------------------------------------------------------

_ = fmap (+1) (*2) 3      -- `7`
_ = fmap (+1) (*2) $ 3    -- `7`
_ = (fmap (+1) (*2)) 3    -- `7`
_ = (+1) . (*2) $ 3       -- `7`

_ = (+2) . (*1) $ 2         -- `4`
_ = fmap (+2) (*1) $ 2      -- `4`
_ = (+2) `fmap` (*1) $ 2    -- `4`

-- -----------------------------------------------------------------------------

-- Remember that `(->)`` takes two arguments and therefore has kind
-- `* -> * -> *`. So, we know upfront that we have to apply one of
-- the type arguments before we can have a `Functor`. With the
-- `Either Functor`, we know that we will lift over the
-- `Either a` and if our function will be applied, it
-- will be applied to the `b` value.

-- With the function type `data (->) a b` the same rule applies: you have to
-- lift over the `(->) a` and only transform the `b` value. The `a` is
-- conventionally called `r` for `Reader` in these instances.

-- `r` is the type of the argument to the function. It is part of the structure
-- being lifted over when we lift over a function, not the value being
-- transformed or mapped over. This leaves the result of the function
-- as the value being transformed. This happens to line up neatly
-- with what function composition is about.

-- -----------------------------------------------------------------------------

-- Let's see how function composition and fmapping line up:

-- 1. We start with the original function types lined up for convenience.
-- (.)  ::              (b -> c) -> (a -> b) -> a -> c
-- fmap :: Functor f => (a -> b) ->  f    a  -> f    b

-- 2. We more clearly mark the resulting function from a composition.
-- (.)  ::              (b -> c) -> (a -> b) -> (a -> c)
-- fmap :: Functor f => (a -> b) ->  f    a  ->  f    b

-- 3. We change the letters used in `fmap` without altering the meaning.
-- (.)  ::              (b -> c) -> (a -> b) -> (a -> c)
-- fmap :: Functor f => (b -> c) ->  f    b  ->  f    c

-- 4. We shift the notation of the parameter functions from infix to prefix.
-- (.)  ::              ((->) b c) -> ((->) a b) -> ((->) a c)
-- fmap :: Functor f => ((->) b c) ->  f      b  ->  f      c

-- 5. We explicitly show partial function application for parameter functions.
-- (.)  ::              ((->) b c) -> (((->) a) b) -> (((->) a) c)
-- fmap :: Functor f => ((->) b c) ->  f        b  ->  f        c

-- Now we see that the functor `f` is `(->) a`.

-- -----------------------------------------------------------------------------

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

-- `compose` is the same as `(.)`.
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- These are similiar.
-- \r -> f (ra r)
-- \x -> f (g x)

ask :: Reader a a
ask = Reader id

-- -----------------------------------------------------------------------------

-- For the `Applicative` instance of functions, `f` specializes into `(->) r`.

-- pure :: Applicative f
--      => a ->         f a
-- pure :: a ->  ((->) r) a
-- pure :: a -> (((->) r) a)
-- pure :: a -> ( (->) r  a)
-- pure :: a -> (r ->     a)
-- pure :: a ->  r ->     a

-- (<*>) :: Applicative f
--       =>         f (a -> b)  ->  f        a  ->  f        b
-- (<*>) ::  ((->) r) (a -> b)  ->  ((->) r) a  ->  ((->) r) b
-- (<*>) :: (((->) r) (a -> b)) -> (((->) r) a) -> (((->) r) b)
-- (<*>) :: ( (->) r  (a -> b)) -> ( (->) r  a) -> ( (->) r  b)
-- (<*>) :: (r ->     (a -> b)) -> (r ->     a) -> (r ->     b)
-- (<*>) :: (r ->      a -> b ) -> (r ->     a) -> (r ->     b)

-- -----------------------------------------------------------------------------

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

(<$->>) :: (a -> b) -> (r -> a) -> (r -> b)
(<$->>) = (<$>)

(<*->>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
(<*->>) = (<*>)

-- with Reader
getDogR' :: Person -> Dog
getDogR' = Dog <$->> dogName <*->> address

-- with Reader, alternate
getDogR'' :: Person -> Dog
getDogR'' = liftA2 Dog dogName address

-- What we're trying to highlight here is that _Reader_ is not always `Reader`,
-- sometimes it's the ambient `Applicative` or `Monad` associated with the
-- partially applied function type, here that is `r ->`.

-- -----------------------------------------------------------------------------

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f aa ab = f <$> aa <*> ab

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

-- -----------------------------------------------------------------------------

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r

-- -----------------------------------------------------------------------------

-- This is how we get to the `Monad` of functions:
--
-- (>>=) :: Monad m
--       =>  m        a  -> (a ->  m        b)  ->  m        b
-- (>>=) ::  ((->) r) a  -> (a ->  ((->) r) b)  ->  ((->) r) b
-- (>>=) :: (((->) r) a) -> (a -> (((->) r) b)) -> (((->) r) b)
-- (>>=) :: ( (->) r  a) -> (a -> ( (->) r  b)) -> ( (->) r  b)
-- (>>=) :: (r ->     a) -> (a -> (r ->     b)) -> (r ->     b)
-- (>>=) :: (r ->     a) -> (a ->  r ->     b)  ->  r ->     b
--
-- return :: Monad m
--        => a ->  m        a
-- return :: a ->  ((->) r) a
-- return :: a -> (((->) r) a)
-- return :: a -> ( (->) r  a)
-- return :: a -> (r ->     a)
-- return :: a ->  r ->     a

-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
-- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)

-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
-- (=<<) :: (a -> r -> b) -> (r -> a) -> (r -> b)

-- -----------------------------------------------------------------------------

-- with Reader Monad
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

-- -----------------------------------------------------------------------------

instance Monad (Reader r) where
  (>>=) r f = join $ f <$> r
  --
  -- Equivalent variations:
  --
  -- (>>=) = (join .) . flip (<$>)
  -- (>>=) (Reader r) f = join $ f <$> Reader r
  -- (>>=) (Reader r) f = join $ Reader (f <$> r)
  -- (>>=) (Reader r) f = join $ Reader $ f <$> r
  --
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  -- (Reader ra) >>= aRb = join $ Reader $ \r -> aRb $ ra r

getDogRM' :: Reader Person Dog
getDogRM' = do
  name <- Reader dogName
  addy <- Reader address
  return $ Dog name addy

-- -----------------------------------------------------------------------------

-- A `Reader` of a primitive type such as `Int` isn't all that useful or
-- compelling. Usually if you have a `Reader`, it's of a record of
-- multiple values that you're getting out of the Reader.

-- -----------------------------------------------------------------------------

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

_ = x1      -- `Just (6, 9)`
_ = x2      -- `Nothing`
_ = x3 3    -- `(Just 9, Just 9)`

_ = s'    -- `Just 15`

_ = fromMaybe 0 xs    -- `6`
_ = fromMaybe 0 zs    -- `0`

tryReader :: IO ()
tryReader = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ foldl (&&) False $ sequA 7
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys

{- GHCi ------------------------------------------------------------------------

> tryReader
Just [3,2,1]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
Just [6,9]
Just 15
Nothing
True
[True,False,False]
[True,True,False]
False
[True,False,False]
False

-------------------------------------------------------------------------------}

-- A monad transformer allows us to combine two monads into one that shares the
-- behaviors of both.

-- It is somewhat common to create a stack of transformers to create one large
-- monad that has features from several monads, for example, rolling `Reader`,
-- `Either`, and `IO` together to get a monad that captures the behavior
-- of waiting for an argument that will get passed around to multiple
-- functions but is likely to come in via some kind of I/O action
-- and has the possibility of failure we might like to catch.
-- Often this stack will be given a type alias
-- for convenience.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

Monad transformer: A type constructor that accepts a monad and gives a monad.

-}
