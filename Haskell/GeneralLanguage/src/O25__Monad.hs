module O25__Monad () where

import Control.Applicative (liftA2, liftA3, (*>))
import Control.Monad (join, liftM2, liftM3, (>=>))

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- This is only here for reference.:
--
-- class Applicative m => Monad m where
--   -- bind
--   (>>=) :: m a -> (a -> m b) -> m b
--   -- sequence
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a

-- fmap f xs == xs >>= return . f

_ = fmap (+1) [1..3]            -- `[2, 3, 4]`
_ = [1..3] >>= return . (+1)    -- `[2, 3, 4]`

-- -----------------------------------------------------------------------------

-- Whenever you've implemented an instance of `Monad` for a type you necessarily
-- have an `Applicative` and a `Functor` as well.

-- -----------------------------------------------------------------------------

-- ($)   ::                    (a ->   b) ->   a ->   b
-- fmap  ::     Functor f =>   (a ->   b) -> f a -> f b
-- (<*>) :: Applicative f => f (a ->   b) -> f a -> f b
-- (=<<) ::       Monad m =>   (a -> m b) -> m a -> m b

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- ($>) ::     Functor f => f a ->   b -> f b
-- (*>) :: Applicative f => f a -> f b -> f b
-- (>>) ::       Monad m => m a -> m b -> m b

andOne x = [x, 1]
_ = andOne 10                         -- `[10, 1]`
_ = fmap andOne [4, 5, 6]             -- `[[4, 1], [5, 1], [6, 1]]`
_ = concat $ fmap andOne [4, 5, 6]    -- `[4, 1, 5, 1, 6, 1]`

-- concat :: Foldable t => t [a] -> [a]

-- `Monad`, in a sense, is a generalization of `concat`.

-- join :: Monad m => m (m a) -> m a

_ = concat [[1], [2, 3], [4, 5, 6]]    -- `[1, 2, 3, 4, 5, 6]`
_ = join [[1], [2, 3], [4, 5, 6]]      -- `[1, 2, 3, 4, 5, 6]`

-- It's by putting that `join` function together with the mapping function that
-- we get "bind", also known as `>>=`.

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

-- fmap  ::     Functor f => (a -> b) -> f a -> f b
-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- liftM ::       Monad m => (a1 -> r) -> m a1 -> m r

_ = liftA2 (,) (Just 3) (Just 5)    -- `Just (3, 5)`
_ = liftM2 (,) (Just 3) (Just 5)    -- `Just (3, 5)`

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

_ = zipWith (+) [3, 4] [5, 6]    -- `[8, 10]`
_ = liftA2 (+) [3, 4] [5, 6]     -- `[8, 9, 9, 10]`
_ = liftM2 (+) [3, 4] [5, 6]     -- `[8, 9, 9, 10]`

-- zipWith3 ::                  (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- liftA3   :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- liftM3   ::       Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r

_ = zipWith3 (,,) [1, 2] [3] [5, 6]    -- `[(1, 3, 5)]`
_ = liftA3 (,,) [1, 2] [3] [5, 6]      -- `[(1, 3, 5), (1, 3, 6), (2, 3, 5), (2, 3, 6)]`
_ = liftM3 (,,) [1, 2] [3] [5, 6]      -- `[(1, 3, 5), (1, 3, 6), (2, 3, 5), (2, 3, 6)]`

-- -----------------------------------------------------------------------------

-- (*>) :: Applicative f => f a -> f b -> f b
-- (>>) ::       Monad m => m a -> m b -> m b


{- GHCi ------------------------------------------------------------------------

> putStrLn "Hello, " *> putStrLn "World!"
Hello
World!

> putStrLn "Hello, " >> putStrLn "World!"
Hello
World!

-------------------------------------------------------------------------------}

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"
--
-- Output:
--
-- blah
-- another thing

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"
--
-- Output:
--
-- blah
-- another thing

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"
--
-- Output:
--
-- blah
-- another thing

-- -----------------------------------------------------------------------------

-- This will wait for user input before printing back what was entered.
binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

-- This behaves the same as `binding`.
binding' :: IO ()
binding' = getLine >>= putStrLn



{- GHCi ------------------------------------------------------------------------

> putStrLn <$> getLine
bladbiddyblah

-------------------------------------------------------------------------------}

-- Mapping `putStrLn` over `getLine` did not work because an additional nested
-- layer of `IO` was introduced during the mapping.

{- GHCi ------------------------------------------------------------------------

> :t getLine
getLine :: IO String

> :t putStrLn
putStrLn :: String -> IO ()

> :t (<$>)
(<$>) :: Functor f => (a -> b) -> f a -> f b

> :t putStrLn <$> getLine
putStrLn <$> getLine :: IO (IO ())

-------------------------------------------------------------------------------}

-- (<$>) :: Functor f => (a      -> b    ) -> f  a      -> f   b
--                       (String -> IO ()) -> IO String -> IO (IO ())

-- To fix this, `join` must be used to join the different layers of structure.

{- GHCi ------------------------------------------------------------------------

> join $ putStrLn <$> getLine
bladbiddyblah
bladbiddyblah

> :t join $ putStrLn <$> getLine
join $ putStrLn <$> getLine :: IO ()

-------------------------------------------------------------------------------}

-- What `join` did here is _merge_ the effects of `getLine` and `putStrLn` into
-- a single `IO` action. This merged `IO` action performs the effects in the
-- order determined by the nesting of the `IO` actions.

-- -----------------------------------------------------------------------------

-- `do` syntax is syntactic sugar used in functions that return monads to
-- sequence operations in a convenient format.

-- The `do` syntax specifically allows us to sequence monadic actions.

-- `bindingAndSequencing` and `bindingAndSequencing'` are equivalent.

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
    \name -> putStrLn ("y helo thar: " ++ name)

-- `twoBinds` and `twoBinds'` are equivalent.

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine

  putStrLn "age pls:"
  age <- getLine

  putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
    \name ->
      putStrLn "age pls:" >>
      getLine >>=
        \age ->
          putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")

-- -----------------------------------------------------------------------------

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else [x*x]

_ = twiceWhenEven [1..3]    -- `[1, 4, 4, 9]`

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else []

_ = twiceWhenEven' [1..3]    -- `[4, 4]`

-- -----------------------------------------------------------------------------

data Cow = Cow
  { name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)

_ = mkSphericalCow "Bess" 5 499    -- `Just (Cow { name = "Bess", age = 5, weight = 499 })`
_ = mkSphericalCow "Bess" 5 500    -- `Nothing`

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

_ = mkSphericalCow' "Bess" 5 499    -- `Just (Cow { name = "Bess", age = 5, weight = 499 })`
_ = mkSphericalCow' "Bess" 5 500    -- `Nothing`

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
  \nammy ->
    noNegative age' >>=
    \agey ->
      noNegative weight' >>=
      \weighty ->
        weightCheck (Cow nammy agey weighty)

_ = mkSphericalCow'' "Bess" 5 499    -- `Just (Cow { name = "Bess", age = 5, weight = 499 })`
_ = mkSphericalCow'' "Bess" 5 500    -- `Nothing`

-- -----------------------------------------------------------------------------

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i + 1)
  else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

-- `do` blocks of the following form can be rewritten using `Applicative`.

doSomething = do
  a <- f
  b <- g
  c <- h
  pure (a, b, c)

-- `do` blocks of the following form require the use of `Monad`.

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

-- -----------------------------------------------------------------------------

-- The `Maybe` monad won't continue with a calculation in the `Nothing` case.

_ = Nothing >>= undefined    -- `Nothing`

{-
_ = Just 1 >>= undefined    -- Error.
-}

-- -----------------------------------------------------------------------------

type Founded = Int
type Coders = Int

data SoftwareShop = Shop
  { founded :: Founded
  , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError
  = NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 5000  = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded     <- validateFounded years
  programmers <- validateCoders coders

  if programmers > div founded 10
  then Left $ TooManyCodersForYears founded programmers
  else Right $ Shop founded programmers

_ = mkSoftware 0 0          -- `Right (Shop { founded = 0, programmers = 0 })`
_ = mkSoftware (-1) 0       -- `Left (NegativeYears (-1))`
_ = mkSoftware (-1) (-1)    -- `Left (NegativeYears (-1))`
_ = mkSoftware 0 (-1)       -- `Left (NegativeCoders (-1))`
_ = mkSoftware 500 0        -- `Right (Shop { founded = 500, programmers = 0 })`
_ = mkSoftware 501 0        -- `Left (TooManyYears 501)`
_ = mkSoftware 501 501      -- `Left (TooManyYears 501)`
_ = mkSoftware 100 5001     -- `Left (TooManyCoders 5001)`
_ = mkSoftware 0 500        -- `Left (TooManyCodersForYears 0 500)`

-- -----------------------------------------------------------------------------

-- `Applicative` and `Monad` instances must have the same behavior.

--  (<*>) == Control.Monad.ap

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- ap    ::       Monad m => m (a -> b) -> m a -> m b

-- -----------------------------------------------------------------------------

data Sum a b = First a | Second b deriving (Eq, Show)

-- TODO:

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second

  (<*>) (First a)  _          = First a
  (<*>) (Second a) (First b)  = (First b)
  (<*>) (Second a) (Second b) = Second (a b)

instance Monad (Sum a) where
  return = pure

  (>>=) (First a)  _ = First a
  (>>=) (Second a) f = join $ Second (f a)

-- -----------------------------------------------------------------------------

-- Monad laws:

-- Identity:
-- right identity: m >>= return = m
-- left identity: x >>= f = f x

-- Associativity:
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)

-- Composition:
--

-- -----------------------------------------------------------------------------

data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0

  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure

  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

mainCount :: IO ()
mainCount = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- -----------------------------------------------------------------------------

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = join (f <$> (g a))

mcomp'' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp'' f g a = g a >>= f

-- -----------------------------------------------------------------------------

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- (>=>)    :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- flip (.) ::            (a ->   b) -> (b ->   c) -> a ->   c

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "

-- -----------------------------------------------------------------------------

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure = pure NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

testNope :: IO ()
testNope = do
  let trigger :: Nope (String, String, String)
      trigger = NopeDotJpg
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- -----------------------------------------------------------------------------

data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a)  = PLeft (f a)

instance Applicative (BahEither b) where
  pure = PLeft
  (<*>) (PRight f) _          = PRight f
  (<*>) _          (PRight a) = PRight a
  (<*>) (PLeft f) (PLeft a)   = PLeft (f a)

instance Monad (BahEither b) where
  (>>=) (PRight a) _ = PRight a
  (>>=) (PLeft a)  f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return $ PLeft a
          , return $ PRight b
          ]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

testBahEither :: IO ()
testBahEither = do
  let trigger :: BahEither (String, String, String) (String, String, String)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- -----------------------------------------------------------------------------

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

testIdentity :: IO ()
testIdentity = do
  let trigger :: Identity (String, String, String)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- -----------------------------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil          = Nil
  fmap f (Cons x xs)  = Cons (f x) (fmap f xs)

appendLists :: List a -> List a -> List a
appendLists Nil         ys = ys
appendLists (Cons x xs) ys = Cons x (appendLists xs ys)

instance Applicative List where
  pure x = Cons x Nil

  (<*>) Nil         _  = Nil
  (<*>) (Cons f fs) xs = appendLists (f <$> xs) (fs <*> xs)

instance Monad List where
  (>>=) Nil         _ = Nil
  (>>=) (Cons x xs) f = appendLists (f x) (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return Nil
          , return $ Cons a b
          ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

testList :: IO ()
testList = do
  let trigger :: List (Int, Int, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- -----------------------------------------------------------------------------

j :: Monad m => m (m a) -> m a
j = join

_ = j [[1, 2], [], [3]]    -- `[1, 2, 3]`
_ = j (Just (Just 1))      -- `Just 1`
_ = j (Just Nothing)       -- `Nothing`
_ = j Nothing              -- `Nothing`

j' :: Monad m => m (m a) -> m a
j' mma = mma >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l1' :: Monad m => (a -> b) -> m a -> m b
l1' f ma = f <$> ma

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' f ma mb = f <$> ma <*> mb

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

a' :: Monad m => m a -> m (a -> b) -> m b
a' ma mf = mf <*> ma

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh []     _ = return []
meh (x:xs) f = (:) <$> f x <*> meh xs f

flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id

-- -----------------------------------------------------------------------------

-- This is only here for reference.
--
-- instance Monad Maybe where
--   return x = Just x
--   (Just x) >>= k = k x
--   Nothing >>= _ = Nothing



-- It is not necessary, and considered bad style, to use `do` in single-line
-- expressions.

-- Similarly, it is unnecessary to use `do` with functions like `putStrLn` and
-- `print` that already have the effects baked in.



-- The `do` keyword introduces the do-block.
concatUserInput = do
  -- `x1` and `x2` are variables bound to user input.
  x1 <- getLine
  x2 <- getLine
  -- `return` wraps its argument in the monad.
  return (x1 ++ x2)

{-
-- Doesn't work:
twoo :: IO Bool
twoo = do
  c <- getChar
  c' <- getChar
  c == c'
-}

-- Works:
twoo' :: IO Bool
twoo' = do
  c <- getChar
  c' <- getChar
  return (c == c')

-- Overusing the `do`:
twoo'' :: IO Bool
twoo'' = do
  c <- getChar
  c' <- getChar
  do return (c == c')

main' :: IO ()
main' = do
  c <- getChar
  c' <- getChar
  if c == c'
  then putStrLn "True"
  else return ()

-- -----------------------------------------------------------------------------

-- fmap  ::   (a ->   b) -> f a -> f b
-- (<*>) :: f (a ->   b) -> f a -> f b
-- (=<<) ::   (a -> f b) -> f a -> f b

-- (.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

-- lifting (a -> b) over f in f a
-- fmap :: (a -> b) -> f a -> f b

-- binding (a -> m b) over m in m a
-- (>>=) :: m a -> (a -> m b) -> m b

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

`do` notation: Syntatic sugar for sequencing function evaluations.
Kleisli composition: Function composition that accounts for monadic structure.
Monad: An applicative functor that can flatten its nested type.
Monadic function: A function outputting more structure when lifted over a monad.
Bind: For a monad, to lift a monadic function over a monadic structure.

-}
