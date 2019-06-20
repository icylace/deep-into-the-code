module O11__PatternMatching () where

-- Pattern matching is a way of matching values against patterns and, where
-- appropriate, binding variables to successful matches.

-- A pattern is a syntactic way of deconstructing product and sum types to get
-- at their inhabitants.

-- Patterns are matched against values, or data constructors, not types.
-- Matching a pattern may fail, procedding to the next available pattern
-- to match or succeed.  When a match succeeds, the variables exposed in
-- the pattern are bound.  Pattern matching proceeds from left to right
-- and outside to inside.

-- The underscore here represents a "catch-all" case that never fails to match.

f :: Integer -> Bool
f 2 = True
f _ = False

_ = f 1     -- `False`
_ = f 2     -- `True`
_ = f 3     -- `False`
_ = f 50    -- `False`

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- The first pattern that is matched against will be used.

f' :: Integer -> Bool
f' _ = False
f' 2 = True
-- Triggers a warning when the `-Woverlapping-patterns` compiler option is used.
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#ghc-flag--Woverlapping-patterns

_ = f' 1     -- `False`
_ = f' 2     -- `False`
_ = f' 3     -- `False`
_ = f' 50    -- `False`

-- The definition of `f'` will incur a warning because its catch-all case is
-- matched first which prevents every other case coming after it from
-- ever matching.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- A function will return "bottom", a non-value, when it's applied to an
-- appropriately typed argument it doesn't know how to handle.

-- Evaluating a function applied to an unhandled argument throws an exception.

-- Functions of this sort are called partial functions and are also said to be
-- incomplete pattern matches or non-exhaustive.

-- A partial function can implicitly not handle all cases.

g :: Integer -> Bool
g 2 = True

_ = g 1     -- Throws an exception.
_ = g 2     -- `True`
_ = g 3     -- Throws an exception.
_ = g 50    -- Throws an exception.

-- -----------------------------------------------------------------------------

-- A partial function can also explicitly not handle all cases.

-- The special `undefined` value is a way to explicitly use bottom.

g' :: Integer -> Bool
g' 2 = True
g' _ = undefined

_ = g' 1    -- Throws an exception.
_ = g' 2    -- `True`
_ = g' 3    -- Throws an exception.
_ = g' 50   -- Throws an exception.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Patterns can also extract data out of matched arguments.

data WherePenguinsLive =
  Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica' :: WherePenguinsLive -> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt  = Peng SouthAmerica
gentoo    = Peng Antarctica
macaroni  = Peng Antarctica
little    = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = (galapagosPenguin p) || (antarcticPenguin p)

-- -----------------------------------------------------------------------------

h :: (a, b) -> (c, d) -> ((b, d), (a, c))
h x y = ((snd x, snd y), (fst x, fst y))

h' :: (a, b) -> (c, d) -> ((b, d), (a, c))
h' (a, b) (c, d) = ((b, d), (a, c))

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------












-- nullary data constructor,
-- not a sum or product.
-- Just a single value.
data Blah = Blah

-- Pattern matching on `Blah` can only do one thing.

blahFunc :: Blah -> Bool
blahFunc Blah = True

-- -----------------------------------------------------------------------------

data Identity a = Identity a deriving (Eq, Show)

-- `Identity` is a unary data constructor.

-- when you pattern match on Identity
-- you can unpack and expose the 'a'

unpackIdentity :: Identity a -> a
unpackIdentity (Identity x) = x

-- But you can choose to ignore
-- the contents of Identity

ignoreIdentity :: Identity a -> Bool
ignoreIdentity (Identity _) = True

-- or ignore it completely since
-- matching on a non-sum data constructor
-- changes nothing.

ignoreIdentity' :: Identity a -> Bool
ignoreIdentity' _ = True

-- -----------------------------------------------------------------------------

data Product a b = Product a b deriving (Eq, Show)

productUnpackOnlyA :: Product a b -> a
productUnpackOnlyA (Product x _) = x

productUnpackOnlyB :: Product a b -> b
productUnpackOnlyB (Product _ y) = y

-- We can bind the `Product` values to different names.

productUnpack :: Product a b -> (a, b)
productUnpack (Product x y) = (x, y)

-- -----------------------------------------------------------------------------

data SumOfThree a b c =
  FirstPossible a
  | SecondPossible b
  | ThirdPossible c
  deriving (Eq, Show)

sumToInt :: SumOfThree a b c -> Integer
sumToInt (FirstPossible _)  = 0
sumToInt (SecondPossible _) = 1
sumToInt (ThirdPossible _)  = 2

-- We can selectively ignore
-- inhabitants of the sum

sumToInt' :: SumOfThree a b c -> Integer
sumToInt' (FirstPossible _) = 0
sumToInt' _                 = 1





-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------


-- Pattern matching is strict by default.



-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------





fst' :: (a, b) -> a
fst' (a, _) = a

snd' :: (a, b) -> b
snd' (_, b) = b

tup :: (Integer, [a]) -> (Integer, [a]) -> (Integer, [a])
tup (a, b) (c, d) = ((a + c), (b ++ d))



-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- TODO:
-- _ = if x + 1 == 1 then "AWESOME" else "wut"

-- A case expression is a way of varying function behavior for certain values.

funcZ :: (Eq a, Num a) => a -> String
funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

_ = funcZ 0   -- `"AWESOME"`
_ = funcZ 1   -- `"wut"`

-- -----------------------------------------------------------------------------

pal :: Eq a => [a] -> String
pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal' :: Eq a => [a] -> String
pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs

-- -----------------------------------------------------------------------------

-- A case expression that is not total.

nums :: (Ord a, Num a, Num p) => a -> p
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
-- Triggers a warning when the `-Wincomplete-patterns` compiler option is used.
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#ghc-flag--Wincomplete-patterns

-- -----------------------------------------------------------------------------

fn :: (Ord a, Num a, Num p) => a -> p
fn x =
  case compare x 0 of
    LT -> -1
    EQ -> 0
    GT -> 1

-- -----------------------------------------------------------------------------

fn x =
  case compare x 0 of
    LT -> -1
    _  -> 2

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- An "as-pattern" in Haskell is a way of referring to a value while
-- simultaneously pattern matching on part of it.

f2 :: Show a => (a, b) -> IO (a, b)
f2 t@(a, _) = do
  print a
  return t

-- -----------------------------------------------------------------------------

doubleUp :: [a] -> [a]
doubleUp []       = []
doubleUp xs@(x:_) = x : xs

_ = doubleUp []           -- `[]`
_ = doubleUp [1]          -- `[1,1]`
_ = doubleUp [1, 2]       -- `[1,1,2]`
_ = doubleUp [1, 2, 3]    -- `[1,1,2,3]`

-- -----------------------------------------------------------------------------




isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf []     _        = True
isSubseqOf _      []       = False
isSubseqOf (x:xs) y@(_:ys) = elem x y && isSubseqOf xs ys

_ = isSubseqOf "blah" "blahwoot"    -- `True`
_ = isSubseqOf "blah" "wootblah"    -- `True`
_ = isSubseqOf "blah" "wboloath"    -- `True`
_ = isSubseqOf "blah" "wootbla"     -- `False`
_ = isSubseqOf "blah" "halbwoot"    -- `False`
_ = isSubseqOf "blah" "blawhoot"    -- `True`





-- -----------------------------------------------------------------------------



{-

import Data.Char (toUpper)

capitalizeWords :: String -> [(String, String)]
capitalizeWords x = zip ws $ map (\(c:cs) -> toUpper c : cs) ws
  where ws = words x

_ = capitalizeWords "hello world"   -- `[("hello","Hello"),("world","World")]`





capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

_ = capitalizeWord "Chortle"    -- `"Chortle"`
_ = capitalizeWord "chortle"    -- `"Chortle"`


-}






{- These work too.
capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph (c:cs) = toUpper c : go cs
  where go (x:y:z:xs) =
          if x == '.' && y == ' '
          then x : y : toUpper z : go xs
          else x : go (y : z : xs)
        go xs = xs

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph (c:cs) = toUpper c : go cs
  where go (x:y:z:xs) =
          if [x, y] == ". "
          then x : y : toUpper z : go xs
          else x : go (y : z : xs)
        go xs = xs

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph (c:cs) = toUpper c : go cs
  where go ('.':' ':z:xs) = '.' : ' ' : toUpper z : go xs
        go (x:xs) = x : go xs
        go xs = xs



capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph (c:cs) = toUpper c : go cs
  where go ('.':' ':z:xs) = ". " ++ toUpper z : go xs
        go (x:xs) = x : go xs
        go xs = xs

_ = capitalizeParagraph "blah. woot ha."    -- `"Blah. Woot ha."`


-}








-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========

-- Pattern matching: Identifying arguments that follow a given pattern.
-- Partial function: A function that does not handle all possible inputs.
-- Total function: A function that does handle all possible inputs.
