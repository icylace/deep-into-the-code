{-# LANGUAGE FlexibleContexts #-}

module O27__Traversable where

-- `Traversable` allows you to transform elements inside the structure like
-- a unctor, producing applicative effects along the way, and lift those
-- potentially multiple instances of applicative structure outside of
-- the traversable structure. It is commonly described as a way
-- to traverse a data structure, mapping a function inside
-- a structure while accumulating the applicative
-- contexts along the way.

import Data.Functor.Constant (Constant(..))
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Maybe (catMaybes)
import Data.Monoid (Sum(..))
import Data.Traversable (sequenceA, traverse)

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (traversable)

-- This is only here for reference.
--
-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   traverse f = sequenceA . fmap f

-- fmap     ::                     Functor f  => (a ->   b) -> f a -> f    b
-- (=<<)    ::                       Monad m  => (a -> m b) -> m a -> m    b
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- myData :: [String]
-- myFunc :: String -> IO Record

-- wrong :: [IO Record]
-- wrong = fmap myFunc myData

-- right :: IO [Record]
-- right = traverse myFunc myData

-- The effect of `sequenceA` is flipping two contexts or structures. It doesn't
-- by itself allow you to apply any function to the `a` value inside the
-- structure; it only flips the layers of structure around.

-- sequenceA :: Applicative f => t (f a) -> f (t a)
-- sequenceA = traverse id

_ = sum [1, 2, 3]                             -- `6`
_ = fmap sum [Just 1, Just 2, Just 3]         -- `[1, 2, 3]`
_ = (fmap . fmap) sum Just [1, 2, 3]          -- `Just 6`
_ = ((<$>) . (<$>)) sum Just [1, 2, 3]        -- `Just 6`
_ = fmap product [Just 1, Just 2, Nothing]    -- `[1, 2, 1]`

_ = fmap Just [1, 2, 3]                                   -- `[Just 1, Just 2, Just 3]`
_ = sequenceA $ Just [1, 2, 3]                            -- `[Just 1, Just 2, Just 3]`
_ = sequenceA [Just 1, Just 2, Just 3]                    -- `Just [1, 2, 3]`
_ = sequenceA $ fmap Just [1, 2, 3]                       -- `Just [1, 2, 3]`
_ = sequenceA [Just 1, Just 2, Nothing]                   -- `Nothing`
_ = fmap sum $ sequenceA [Just 1, Just 2, Just 3]         -- `Just 6`
_ = fmap product $ sequenceA [Just 1, Just 2, Nothing]    -- `Nothing`

-- -----------------------------------------------------------------------------

-- `catMaybe` concatenates `Just` values from a list of `Maybe` values.

xs = [Just 1, Just 2, Just 3]
_ = catMaybes xs    -- `[1, 2, 3]`

xsn = [Just 1, Just 2, Nothing]
_ = catMaybes xsn    -- `[1, 2]`

xsn' = xs ++ [Nothing]
_ = sum $ catMaybes xsn'         -- `6`
_ = fmap sum $ sequenceA xsn'    -- `Nothing`

-- -----------------------------------------------------------------------------

-- With `traverse` We're still mapping a function over some embedded value(s),
-- like `fmap`, but similar to flip bind (`=<<`), that function is itself
-- generating more structure. However, unlike flip bind, that structure
-- can be of a different type than the structure we lifted over to
-- apply the function. And at the end, it will flip the two
-- structures around, as `sequenceA` did.
--
-- traverse f = sequenceA . fmap f

_ = fmap Just [1, 2, 3]                  -- `[Just 1, Just 2, Just 3]`
_ = sequenceA $ fmap Just [1, 2, 3]      -- `Just [1, 2, 3]`
_ = sequenceA . fmap Just $ [1, 2, 3]    -- `Just [1, 2, 3]`
_ = traverse Just [1, 2, 3]              -- `Just [1, 2, 3]`

-- In a literal sense, anytime you need to flip two type constructors around,
-- or map something and then flip them around, that's probably `Traversable`.

-- -----------------------------------------------------------------------------

-- `Traversable` is stronger than `Functor` and `Foldable`. Because of this, we
-- can recover the `Functor` and `Foldable` instance for a type from the
-- `Traversable`, just as we can recover the `Functor` and `Applicative`
-- from the `Monad`. Here we can use the `Identity` type to get
-- something that is essentially just `fmap` all over again

_ = traverse (Identity . (+1)) [1, 2]                  -- `Identity [2, 3]`
_ = runIdentity $ traverse (Identity . (+1)) [1, 2]    -- `[2, 3]`

edgeMap :: Traversable t => (a -> b) -> t a -> t b
edgeMap f t = runIdentity $ traverse (Identity . f) t

_ = edgeMap (+1) [1..5]    -- `[2, 3, 4, 5, 6]`

-- -----------------------------------------------------------------------------


ys = [1, 2, 3, 4, 5] :: [Sum Integer]

_ = traverse (Constant . (+1)) ys    -- `Constant (Sum { getSum = 20 })`

foldMap' :: (Traversable t, Monoid a) => (a1 -> a) -> t a1 -> a
foldMap' f t = getConstant $ traverse (Constant . f) t

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

-- -----------------------------------------------------------------------------

-- Traversable laws:

-- -----------------------------------------------------------------------------

-- Naturality (for `traverse`):
-- t . traverse f = traverse (t . f)

-- This law tells us that function composition behaves in unsurprising ways with
-- respect to a traversed function. Since a traversed function `f` is generating
-- the structure that appears on the "outside" of the `traverse` operation,
-- there's no reason we shouldn't be able to float a function over the
-- structure into the traversal itself.

-- -----------------------------------------------------------------------------

-- Identity (for `traverse`):
-- traverse Identity = Identity

-- This law says that traversing the data constructor of the `Identity` type
-- over a value will produce the same result as just putting the value in
-- `Identity`. This tells us `Identity` represents a structural identity
-- for traversing data. This is another way of saying that a
-- `Traversable` instance cannot add or inject any
-- structure or effects.

-- -----------------------------------------------------------------------------

-- Composition (for `traverse`):
-- traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f

-- This law demonstrates how we can collapse sequential traversals into a single
-- traversal, by taking advantage of the `Compose` datatype, which
-- combines structure.

-- -----------------------------------------------------------------------------

-- Naturality (for `sequenceA):
-- t . sequenceA = sequenceA . fmap t

-- Identity (for `sequenceA):
-- sequenceA . fmap Identity = Identity

-- Composition (for `sequenceA):
-- sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA

-- -----------------------------------------------------------------------------

type TI = []

testTI :: IO ()
testTI = do
  let trigger :: TI (Int, Int, [Int])
      trigger = undefined
  quickBatch $ traversable trigger

-- -----------------------------------------------------------------------------

newtype Identity' a = Identity' a deriving (Eq, Show)

instance Functor Identity' where
  fmap f (Identity' a) = Identity' $ f a

instance Foldable Identity' where
  foldMap f (Identity' a) = f a

instance Traversable Identity' where
  traverse f (Identity' a) = Identity' <$> f a

instance Arbitrary a => Arbitrary (Identity' a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity' a

instance Eq a => EqProp (Identity' a) where
  (=-=) = eq

testIdentity' :: IO ()
testIdentity' = do
  let trigger :: Identity' (String, String, String)
      trigger = undefined
  quickBatch $ traversable trigger

-- -----------------------------------------------------------------------------

newtype Constant' a b = Constant' { getConstant' :: a } deriving (Eq, Show)

instance Functor (Constant' a) where
  fmap _ (Constant' a) = Constant' a

instance Monoid a => Applicative (Constant' a) where
  pure _ = Constant' mempty
  (<*>) (Constant' f) (Constant' a) = Constant' $ f <> a

instance Monoid a => Foldable (Constant' a) where
  foldMap _ _ = mempty

instance Monoid a => Traversable (Constant' a) where
  traverse _ (Constant' a) = pure $ Constant' a

instance Arbitrary a => Arbitrary (Constant' a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant' a

instance (Eq a, Eq b) => EqProp (Constant' a b) where
  (=-=) = eq

testConstant' :: IO ()
testConstant' = do
  let trigger :: Constant' (String, String, String) (String, String, String)
      trigger = undefined
  quickBatch $ traversable trigger

-- -----------------------------------------------------------------------------

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    oneof [ return Nada
          , return $ Yep a
          ]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

testOptional :: IO ()
testOptional = do
  let trigger :: Optional (String, String, String)
      trigger = undefined
  quickBatch $ traversable trigger

-- -----------------------------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil      = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Foldable List where
  foldMap _ Nil        = mempty
  foldMap f (Cons a l) = f a <> foldMap f l

instance Traversable List where
  traverse _ Nil        = pure Nil
  traverse f (Cons a l) = Cons <$> (f a) <*> traverse f l

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    oneof [ return Nil
          , return $ Cons a l
          ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

testList :: IO ()
testList = do
  let trigger :: List (String, String, String)
      trigger = undefined
  quickBatch $ traversable trigger

-- -----------------------------------------------------------------------------

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

testThree :: IO ()
testThree = do
  let trigger :: Three (String, String, String) (String, String, String) (String, String, String)
      trigger = undefined
  quickBatch $ traversable trigger

-- -----------------------------------------------------------------------------

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

testPair :: IO ()
testPair = do
  let trigger :: Pair (String, String, String) (String, String, String)
      trigger = undefined
  quickBatch $ traversable trigger

-- -----------------------------------------------------------------------------

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Big a b c

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

testBig :: IO ()
testBig = do
  let trigger :: Big (String, String, String) (String, String, String)
      trigger = undefined
  quickBatch $ traversable trigger

-- -----------------------------------------------------------------------------

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Bigger a b c d

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

testBigger :: IO ()
testBigger = do
  let trigger :: Bigger (String, String, String) (String, String, String)
      trigger = undefined
  quickBatch $ traversable trigger

-- -----------------------------------------------------------------------------

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
  (=-=) = eq

sampleS :: IO [S [] Int]
sampleS = sample' (arbitrary :: Gen (S [] Int))

testS :: IO ()
testS = do
  let trigger :: S [] (String, String, String)
      trigger = undefined
  quickBatch $ traversable trigger

-- -----------------------------------------------------------------------------

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty           = Empty
  fmap f (Leaf a)        = Leaf (f a)
  fmap f (Node ta a ta') = Node (fmap f ta) (f a) (fmap f ta')

instance Foldable Tree where
  foldMap _ Empty           = mempty
  foldMap f (Leaf a)        = f a
  foldMap f (Node ta a ta') = foldMap f ta <> f a <> foldMap f ta'

instance Traversable Tree where
  traverse _ Empty           = pure Empty
  traverse f (Leaf a)        = Leaf <$> f a
  traverse f (Node ta a ta') = Node <$> (traverse f ta) <*> f a <*> traverse f ta'

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    oneof [ return Empty
          , return $ Leaf a
          , return $ Node (Leaf a) b (Leaf c)
          ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

testTree :: IO ()
testTree = do
  let trigger :: Tree (String, String, String)
      trigger = undefined
  quickBatch $ traversable trigger
