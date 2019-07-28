{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module O23__Functor (functorTests) where

import Test.QuickCheck

-- A functor is a way to apply a function over or around some structure that we
-- don't want to alter. That is, we want to apply the function to the value
-- that is "inside" some structure and leave the structure alone.

-- `Functor` is a type class for function application "over", or "through",
-- some structure `f` that we want to ignore and leave untouched.

-- This is only used for reference.
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

_ = map (\x -> x > 3) [1..6]     -- `[False, False, False, True, True, True]`
_ = fmap (\x -> x > 3) [1..6]    -- `[False, False, False, True, True, True]`

{-
_ = map (+ 1) (Just 1)    -- Error.
-}

_ = fmap (+ 1) (Just 1)                -- `Just 2`
_ = fmap (10 /) (4, 5)                 -- `(4, 2.0)`
_ = fmap (++ " there") (Right "Hi")    -- `Right "Hi there"`

-- `<$>` is the infix alias for `fmap`.

_ = (\x -> x > 3) <$> [1..6]          -- `[False, False, False, True, True, True]`
_ = (+ 1) <$> (Just 1)                -- `Just 2`
_ = (10 /) <$> (4, 5)                 -- `(4, 2.0)`
_ = (++ " there") <$> (Right "Hi")    -- `Right "Hi there"`

-- -----------------------------------------------------------------------------

-- All instances of `Functor` must abide by the functor laws.

-- Identity:
-- fmap id == id

_ = fmap id "Hi Julie"    -- `"Hi Julie"`
_ = id "Hi Julie"         -- `"Hi Julie"`

-- Composition:
-- fmap (f . g) == fmap f . fmap g

_ = fmap ((+ 1) . (* 2)) [1..5]         -- `[3, 5, 7, 9, 11]`
_ = fmap (+ 1) . fmap (* 2) $ [1..5]    -- `[3, 5, 7, 9, 11]`

-- -----------------------------------------------------------------------------

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)

-- A law-abiding instance of `Functor`.
instance Functor WhoCares where
  fmap _ ItDoesnt         = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a)       = Matter (f a)

-- -----------------------------------------------------------------------------

data CountingBad a = Heisenberg Int a deriving (Eq, Show)

-- A law-breaking instance of `Functor`.
instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)

oneWhoKnocks = Heisenberg 0 "Uncle"
f = (<> " Jesse")
g = (<> " lol")

_ = fmap f oneWhoKnocks               -- `Heisenberg 1 "Uncle Jesse"`
_ = fmap g oneWhoKnocks               -- `Heisenberg 1 "Uncle lol"`
_ = fmap (f . g) oneWhoKnocks         -- `Heisenberg 1 "Uncle lol Jesse"`
_ = fmap f . fmap g $ oneWhoKnocks    -- `Heisenberg 2 "Uncle lol Jesse"`

-- The functor composition law was broken because the mapping of `f` caused
-- changes to data that is unrelated to what `f` is actually applied to.

-- -----------------------------------------------------------------------------

data CountingBad' a = Heisenberg' Int a deriving (Eq, Show)

-- A law-abiding instance of `Functor`.
instance Functor CountingBad' where
  fmap f (Heisenberg' n a) = Heisenberg' n (f a)

oneWhoKnocks' = Heisenberg' 0 "Uncle"
f' = (<> " Jesse")
g' = (<> " lol")

_ = fmap f' oneWhoKnocks'                -- `Heisenberg' 0 "Uncle Jesse"`
_ = fmap g' oneWhoKnocks'                -- `Heisenberg' 0 "Uncle lol"`
_ = fmap (f' . g') oneWhoKnocks'         -- `Heisenberg' 0 "Uncle lol Jesse"`
_ = fmap f' . fmap g' $ oneWhoKnocks'    -- `Heisenberg' 0 "Uncle lol Jesse"`

-- The functor composition law was followed because the mapping of `f` caused
-- changes to only what `f` is actually applied to.

-- -----------------------------------------------------------------------------

-- Functions have an instance of `Functor` with their mapping as composition.

_ = fmap (+ 1) negate $ 5    -- `-4`
_ = (+ 1) <$> negate $ 5     -- `-4`
_ = (+ 1) . negate $ 5       -- `-4`

-- -----------------------------------------------------------------------------

-- Nested functors can be mapped at different "depths".

p = const 'p'

x = [Just "Ave", Nothing, Just "woohoo"]

_ = p x                         -- `p`
_ = fmap p x                    -- `"ppp"`
_ = (fmap . fmap) p x           -- `[Just 'p', Nothing, Just 'p']`
_ = (fmap . fmap . fmap) p x    -- `[Just "ppp", Nothing, Just "pppppp"]`

y = [Just ["Ha", "Ha"], Nothing, Just []]

_ = p y                                -- `p`
_ = fmap p y                           -- `"ppp"`
_ = (fmap . fmap) p y                  -- `[Just 'p', Nothing, Just 'p']`
_ = (fmap . fmap . fmap) p y           -- `[Just "pp", Nothing, Just ""]`
_ = (fmap . fmap . fmap . fmap) p y    -- `[Just ["pp", "pp"], Nothing, Just []]`

-- -----------------------------------------------------------------------------

-- But what if you do want a function that can change the value and the
-- structure? Just use a normal function to do that.

-- The point of Functor is to reify and be able to talk about cases where we
-- want to reuse functions in the presence of more structure and be
-- transparently oblivious to that additional structure.

-- -----------------------------------------------------------------------------

_ = (+ 1) <$> read "[1]" :: [Int]                         -- `[2]`
_ = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])      -- `Just ["Hi,lol", "Hellolol"]`
_ = (* 2) . (\x -> x - 2) $ 1                             -- `-2`
_ = ((return '1' ++) . show) <$> (\x -> [x, 1..3]) $ 0    -- `"1[0, 1, 2, 3]"`

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read . ("123" ++) . show <$> ioi
    in (* 3) <$> changed
_ = e    -- `3693`

-- -----------------------------------------------------------------------------

data Two a b = Two a b deriving (Eq, Show)
data Or a b = First a | Second b deriving (Eq, Show)

{-
-- `Two` and `Or` both have kind `* -> * -> *>` but `Functor` requires `* -> *`.
instance Functor Two where    -- Error.
  fmap = undefined
instance Functor Or where    -- Error.
  fmap = undefined
-}

{-
-- Any types included as part of the functorial structure can't be applied to.
instance Functor (Two a) where
  fmap f (Two x y) = Two $ (f x) (f y)    -- Error.
-}

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Functor (Or a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)

-- -----------------------------------------------------------------------------

data Trivial       = Trivial       deriving (Eq, Show)
newtype Identity a = Identity a    deriving (Eq, Show)
data Pair a        = Pair a a      deriving (Eq, Show)
data Couple a b    = Couple a b    deriving (Eq, Show)
data Three a b c   = Three a b c   deriving (Eq, Show)
data Three' a b    = Three' a b b  deriving (Eq, Show)
data Four a b c d  = Four a b c d  deriving (Eq, Show)
data Four' a b     = Four' a a a b deriving (Eq, Show)

{-
-- A functor must contain something that can be mapped over.
instance Functor Trivial where    -- Error.
  fmap = undefined
-}

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Functor (Couple a) where
  fmap f (Couple x y) = Couple x (f y)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

-- -----------------------------------------------------------------------------

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- -----------------------------------------------------------------------------

w :: [Int] -> Bool
w x = functorIdentity x

w' x = functorCompose (+ 1) (* 2) $ (x :: [Int])

-- -----------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

w1 :: Identity Int -> Bool
w1 x = functorIdentity x

w1' x = functorCompose (+ 1) (* 2) $ (x :: Identity Int)

-- -----------------------------------------------------------------------------

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    return (Pair x x)

w2 :: Pair Int -> Bool
w2 x = functorIdentity x

w2' x = functorCompose (+ 1) (* 2) $ (x :: Pair Int)

-- -----------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (Couple a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Couple x y)

w3 :: Couple Int Float -> Bool
w3 x = functorIdentity x

w3' x = functorCompose (+ 1) (* 2) $ (x :: Couple Int Float)

-- -----------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

w4 :: Three Int Float Double -> Bool
w4 x = functorIdentity x

w4' x = functorCompose (+ 1) (* 2) $ (x :: Three Int Float Double)

-- -----------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three' x y z)

w5 :: Three' Int Double -> Bool
w5 x = functorIdentity x

w5' x = functorCompose (+ 1) (* 2) $ (x :: Three' Int Double)

-- -----------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four w x y z)

w6 :: Four Int Bool Float Double -> Bool
w6 x = functorIdentity x

w6' x = functorCompose (+ 1) (* 2) $ (x :: Four Int Bool Float Double)

-- -----------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four' w x y z)

w7 :: Four' Int Integer -> Bool
w7 x = functorIdentity x

w7' x = functorCompose (+ 1) (* 2) $ (x :: Four' Int Integer)

-- -----------------------------------------------------------------------------

functorTests :: IO ()
functorTests = do
  quickCheck w
  quickCheck w'
  quickCheck w1
  quickCheck w1'
  quickCheck w2
  quickCheck w2'
  quickCheck w3
  quickCheck w3'
  quickCheck w4
  quickCheck w4'
  quickCheck w5
  quickCheck w5'
  quickCheck w6
  quickCheck w6'
  quickCheck w7
  quickCheck w7'

-- -----------------------------------------------------------------------------

-- Because of the way `Functor` instances work, the ones for `Maybe` and
-- `Either` ignore the "left" cases, which are typically used for errors
-- or failures. Since `fmap` doesn't touch those cases you can map your
-- function directly to the values you intend to work with and ignore
-- the failure cases.

-- -----------------------------------------------------------------------------

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust Nothing = Nothing
incIfJust (Just n) = Just $ n + 1

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust Nothing = Nothing
showIfJust (Just s) = Just $ show s

-- -----------------------------------------------------------------------------

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+ 1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s

_ = incMaybe (Just 1)                   -- `Just 2`
_ = incMaybe (Nothing :: Maybe Int)     -- `Nothing`
_ = showMaybe (Just 9001)               -- `Just "9001"`
_ = showMaybe (Nothing :: Maybe Int)    -- `Nothing`

-- -----------------------------------------------------------------------------

incMaybe' :: Num a => Maybe a -> Maybe a
incMaybe' = fmap (+ 1)

showMaybe' :: Show a => Maybe a -> Maybe String
showMaybe' = fmap show

_ = incMaybe' (Just 1)                   -- `Just 2`
_ = incMaybe' (Nothing :: Maybe Int)     -- `Nothing`
_ = showMaybe' (Just 9001)               -- `Just "9001"`
_ = showMaybe' (Nothing :: Maybe Int)    -- `Nothing`

-- -----------------------------------------------------------------------------

liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+ 1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

_ = liftedInc (Just 1)                   -- `Just 2`
_ = liftedInc (Nothing :: Maybe Int)     -- `Nothing`
_ = liftedShow (Just 9001)               -- `Just "9001"`
_ = liftedShow (Nothing :: Maybe Int)    -- `Nothing`

_ = liftedInc [1..5]     -- `[2, 3, 4, 5, 6]`
_ = liftedShow [1..5]    -- `["1", "2", "3", "4", "5"]`

-- -----------------------------------------------------------------------------

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

-- -----------------------------------------------------------------------------

incIfRight :: Num a => Either e a -> Either e a
incIfRight (Left e) = Left e
incIfRight (Right n) = Right $ n + 1

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Left e) = Left e
showIfRight (Right s) = Right $ show s

-- -----------------------------------------------------------------------------

incEither :: Num a => Either e a -> Either e a
incEither m = fmap (+1) m

showEither :: Show a => Either e a -> Either e String
showEither s = fmap show s

-- -----------------------------------------------------------------------------

incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+1)

showEither' :: Show a => Either e a -> Either e String
showEither' = fmap show

-- -----------------------------------------------------------------------------

data Sum' a b = First' a | Second' b deriving (Eq, Show)

instance Functor (Sum' a) where
  fmap _ (First' x) = First' x
  fmap f (Second' x) = Second' (f x)

-- -----------------------------------------------------------------------------

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v

_ = const 2 (getConstant (Constant 3))                -- `2`
_ = fmap (const 2) (Constant 3)                       -- `Constant { getConstant = 3 }
_ = getConstant $ fmap (const 2) $ Constant 3         -- `3`
_ = getConstant $ fmap (const "blah") $ Constant 3    -- `3`

-- When you `fmap` `const` over `Constant`, the first argument to `const` is
-- unused because the partially applied `const` is itself unused. The first
-- type argument to `Constant`'s type constructor is in the part of the
-- structure that `Functor` skips over. The second argument to the
-- `Constant` type constructor is the phantom type variable `b`
-- which has no value in the datatype. Since there are no
-- values of the type the `Functor` is supposed to be
-- mapping, we have nothing to apply the function
-- to, so we never use the `const` expressions.

_ = getConstant (id (Constant 3))         -- `3`
_ = getConstant (fmap id (Constant 3))    -- `3`

_ = ((const 3) . (const 5)) 10    -- `3`
_ = ((const 5) . (const 3)) 10    -- `5`

separate = fmap (const 3) . fmap (const 5)
fused = fmap (const 3 . const 5)
_ = getConstant $ separate $ Constant "WOOHOO"    -- `"WOOHOO"`
_ = getConstant $ fused $ Constant "Dogs rule"    -- `"Dogs rule"`

-- -----------------------------------------------------------------------------

-- The structure of our types may require that we also have a `Functor` instance
-- for an intermediate type layer.

data Wrap f a = Wrap (f a) deriving (Eq, Show)

{-
-- This won't work because there's this `f` that we're not hopping over, and `a`
-- (the value fmap should be applying the function to) is an argument to that
-- `f` - the function can't apply to that `f` that is wrapping `a`.
instance Functor (Wrap f) where
  fmap f (Wrap x) = Wrap (f x)    -- Error.
-}

{-
-- Here we don't know what type `f` is and it could be anything, but it needs to
-- be a type that has a `Functor` instance so that we can fmap over it.
instance Functor (Wrap f) where
  fmap f (Wrap x) = Wrap (fmap f x)    -- Error.
-}

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap x) = Wrap (fmap f x)

_ = fmap (+1) (Wrap (Just 1))     -- `Wrap (Just 2)`
_ = fmap (+1) (Wrap [1, 2, 3])    -- `Wrap [2, 3, 4]`

{-
_ = fmap (+1) (Wrap 1)    -- Error.
-}

-- -----------------------------------------------------------------------------

-- A _natural transformation_ is like the opposite of what a functor does. It
-- transforms structure while leaving the structure's contents alone.

-- This type is impossible because we can't have higher-kinded types as argument
-- types to the function type.
--
-- nat :: (f -> g) -> f a -> g a

-- `RankNTypes` makes this work.
type Nat f g = forall a. f a -> g a

-- The quantification of `a` in the righthand side of the declaration forces all
-- functions of this type to be oblivious to the contents of the structures `f`
-- and `g` in much the same way that the identity function cannot do anything
-- but return the argument it was given.

-- Syntactically, it lets us avoid talking about `a` in the type of `Nat`.
-- Which is good since we shouldn't have any specific information about
-- the contents of `f` and `g` because we're supposed to be only
-- performing a structural transformation, not a fold.

-- `RankNTypes` language extensions is needed to quantify `a`.

{-
-- The `a` must be quantified to bring it into scope.
type Nat f g = f a -> g a    -- Error.
-}

-- -----------------------------------------------------------------------------

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

{-
-- A natural transformation cannot process a structure's values...
degenerateMtl :: Nat Maybe []
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a + 1]    -- Error.
-}

{-
-- ...even if we describe the values in a way that might let them be worked on.
degenerateMtl :: Num a => Nat Maybe [] a    -- Error.
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a + 1]
-}

-- -----------------------------------------------------------------------------

-- Using a fold is the appropriate operation to use when both the structure and
-- its contents need to be worked on simultaneously.

-- A fold is like a combination of a functor and a natural transformation.

-- -----------------------------------------------------------------------------

data Tuple a b = Tuple a b deriving (Eq, Show)

-- In Haskell, `Functor` instances are unique for a given datatype.

-- In this Haskell-ish pseudocode, we show that type constructors are applied in
-- order of definition thereby limiting what can be defined as the structure
-- a functor can act upon.
--
-- -- This is impossible in Haskell.
-- instance Functor (Tuple ? b) where
--   fmap f (Tuple a b) = Tuple (f a) b

-- A workaround is to flip the arguments to the type constructor.

-- The other workaround is to make a new type using a `Flip` newtype.
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

-- `FlexibleInstances` makes this work.
instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

_ = fmap (+1) (Flip (Tuple 1 "blah"))    -- `Flip (Tuple 2 "blah")`

-- -----------------------------------------------------------------------------

data More a b = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L a (f b) a'
  fmap f (R b a b') = R (f b) a (f b')

_ = fmap (+1) (L 1 2 3)    -- `L 2 2 4`
_ = fmap (+1) (R 1 2 3)    -- `R 1 3 3`

-- -----------------------------------------------------------------------------

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap f (Desk x)  = Desk x
  fmap f (Bloor y) = Bloor (f y)

-- -----------------------------------------------------------------------------

data K a b = K a

instance Functor (K a) where
  fmap _ (K x) = (K x)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))

-- -----------------------------------------------------------------------------

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

-- -----------------------------------------------------------------------------

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (fmap f x)

-- -----------------------------------------------------------------------------

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

-- -----------------------------------------------------------------------------

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

-- -----------------------------------------------------------------------------

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)

-- -----------------------------------------------------------------------------

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

-- -----------------------------------------------------------------------------

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats
      (GoatLord a)
      (GoatLord a)
      (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat x)       = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- -----------------------------------------------------------------------------

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read g)    = Read (fmap f g)

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

Functor: A mapping between categories.
Functor: A way to apply a function over a structure we want to preserve.
Lift: To preserve a structure while applying a function over its values.
Natural transformation: A way of changing structure while preserving its values.

Higher-kinded polymorphism: Polymorphism with types of a higher kind.

-}
