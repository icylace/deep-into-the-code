-- TODO:


{-# LANGUAGE PartialTypeSignatures #-}




module O00__unsorted where

import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isJust)








-- The main executable in a Haskell program must always have the type IO ().





-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Backwards composition (`.`)
-- Forwards composition (`&`)

{-
import Data.Function (&)

f = (+) 1
g = (*) 10

a = f . g
b = f & g    -- Error.

_ = a 2
_ = b 2
-}

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------









-- The arrow, `->`, is the function type constructor. It's like other type
-- constructors but it takes arguments while having no data constructors.

-- The values of the function type are functions.

-- `->` is also a right-associative infix operator.

-- In contrast, function application is left-associative.











{-

Commands which should be typed at the command line will be preceded by a dollar symbol:

$ stack build

Commands which should be typed at the GHCi interactive mode prompt will be preceded by an angle bracket:

> 1 + 2
3

-}















-- A module is a grouping of code on the file level.

-- The group consists of top-level declarations that may be exported to any
-- other part of the program that exist in other source code files.







-- The compiler gives the least specific and most general type it can.








-- "Bottom" is an indication of a computation that does not result in a value.
-- Computations that failed with an error or failed to terminate like an
-- infinite loop are examples of bottom.

-- If you apply this to any values,
-- it'll recurse indefinitely.
indefinite x = indefinite x

-- It'll a'splode if you pass a False value
dontDoThis :: Bool -> Int
dontDoThis True = 1

-- morally equivalent to
definitelyDontDoThis :: Bool -> Int
definitelyDontDoThis True = 1
definitelyDontDoThis False = error "oops"








-- `undefined` is a special value that can be used to allow compilation to
-- happen for code yet to be implemented. Evaluating "undefined" code will
-- throw an exception. Thanks to lazy evaluation, such code can coexist
-- with normal code.

xx = undefined
-- `xx` is now undefined.

-- `undefined` is another way of expressing bottom.

-- Undefined code can still be typechecked.

-- Since `->` is right-associative, currying is available by default, and
-- function type signatures are implicitly parenthesized.

f1 :: a -> a -> a
f1 = undefined

f2 :: a -> (a -> a)
f2 = undefined

-- `f1` and `f2` have the same types.













-- `curry` nests an unnested function.

first = curry fst

_ = fst (2, 3)     -- `2`
_ = first 2 3      -- `2`
_ = (first 2) 3    -- `2`







-- `uncurry` unnests a nested function.

addUsingPair = uncurry (+)

_ = 2 + 3                  -- `5`
_ = (+) 2 3                -- `5`
_ = addUsingPair (2, 3)    -- `5`









curriedStyle x y      = x + y
uncurriedStyle (x, y) = x + y
anonymousStyle        = \x y -> x + y
nestedAnonymousStyle  = \x -> \y -> x + y

_ = curriedStyle 2 3            -- `5`
_ = uncurriedStyle (2, 3)       -- `5`
_ = anonymousStyle 2 3          -- `5`
_ = nestedAnonymousStyle 2 3    -- `5`












ff :: a -> a -> a -> a
ff = undefined

yy :: Char
yy = undefined

-- The type of `ff x` is `Char -> Char -> Char`.


gg :: a -> b -> c -> b
gg = undefined

_ = gg 0 'c' "woot"
-- Type is `Char`.

hh :: (Num a, Num b) => a -> b -> b
hh = undefined

_ = hh 1.0 2
-- Type is `Num b => b`.

_ = hh 1 (5.5 :: Double)
-- Type is `Double`.


jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined


_ = jackal "keyboard" "has the word jackal in it"
-- Type is `[Char]`.
















add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

_ = add 1 0                                           -- `1`
_ = addOne 0                                          -- `1`
_ = addOnePF 0                                        -- `1`
_ = (addOne . addOne) 0                               -- `2`
_ = (addOnePF . addOne) 0                             -- `2`
_ = (addOne . addOnePF) 0                             -- `2`
_ = (addOnePF . addOnePF) 0                           -- `2`
_ = negate (addOne 0)                                 -- `-1`
_ = (negate . addOne) 0                               -- `-1`
_ = (addOne . addOne . addOne . negate . addOne) 0    -- `2`









-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Anything in normal form is also in weak head normal form (WHNF).

-- This is in normal form because it's fully reduced.

_ = (1, 2)

-- This is in WHNF because the outer expression, the tuple, is fully reduced
-- but one of its subexpressions has an addition operator that hasn't been
-- evaluated yet.

_ = (1, 1 + 1)

-- This function is in normal form because it must be applied to an argument in
-- order to evaluate further.

_ = \x -> x * 10

-- This is neither normal form nor WHNF because the append operator is fully
-- applied but not evaluated.

_ = "Papu" ++ "chon"

-- This is in WHNF because the outer expression, the tuple, is fully reduced
-- but one of its subexpressions has an append operator that hasn't been
-- evaluated yet.

_ = (1, "Papu" ++ "chon")

-- This list is in normal form because all its contained values are known.

_ = [1, 2, 3]

-- The following is an example of WHNF evaluation. `myNum` is in WHNF because
-- it's a list constructed by a range that will only evaluate as far as it
-- has to. `take 2` forces some evaluation of the range. `:sprint`
-- displays what has been evaluated (the numbers) and what
-- hasn't (the underscore).

-- Prelude> let myNum :: [Int]; myNum = [1..10]
--
-- Prelude> :sprint myNum
-- myNum = _
--
-- Prelude> take 2 myNum
-- [1,2]
--
-- Prelude> :sprint myNum
-- myNum = 1 : 2 : _

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------





_ = all even [2, 4, 6]    -- `True`
_ = all even [2, 4, 7]    -- `False`

_ = all isJust [Just 'd', Nothing, Just 'g']     -- `False`
_ = all isJust [Just 'd', Just 'o', Just 'g']    -- `True`



_ = intersperse ' ' "Blah"     -- `"B l a h"`
_ = intersperse 0 [1, 1, 1]    -- `[1, 0, 1, 0, 1]`





-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------





-- The fundamental way to think about evaluation in Haskell is as substitution.







-- {-




{-

_ = jackal "keyboard"
-- Type is `Eq b => b -> [Char]`.

-}





-- We can inspect the type of a function application even if the function
-- is undefined.

{- GHCi ------------------------------------------------------------------------

kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined

> :t kessel 1 2
kessel 1 2 :: (Ord a, Num a) => a

> :t kessel 1 (2 :: Integer)
kessel 1 (2 :: Integer) :: (Ord a, Num a) => a

> :t kessel (1 :: Integer) 2
kessel (1 :: Integer) 2 :: Integer

-------------------------------------------------------------------------------}












-- The composition operator, `.`, takes a couple functions and composes them to
-- make a new function.

-- Prelude> :i (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c    -- Defined in 'GHC.Base'
-- infixr 9 .

g1 = negate . sum

_ = g1 [1, 2, 3, 4, 5]    -- `-15`

_ = negate . sum $ [1, 2, 3, 4, 5]    -- `-15`
_ = negate (sum [1, 2, 3, 4, 5])      -- `-15`
_ = (negate . sum) [1, 2, 3, 4, 5]    -- `-15`

_ = take 5 . filter odd . enumFrom $ 3    -- `[3, 5, 7, 9, 11]`






-- https://stackoverflow.com/a/13147064
-- https://stackoverflow.com/a/2465059
-- https://stackoverflow.com/a/1983310

f = (+)
g = (*)
h = (/)
x = 5
y = 7

_ = f x y
_ = (f x) y
_ = (x `f`) y
_ = (`f` y) x
_ = x `f` y

_ = f $ g x y
-- _ = (f . g) x y
-- _ = f (g x) y
_ = f . g x $ y
_ = (f . g x) y

-- _ = f $ g $ h x
-- _ = (f . g . h) x
-- _ = f . g . h $ x

-- _ = f . g . h
-- _ = (f . g) . h
-- _ = f . (g . h)







{-


(f . g) x = f (g x)

f . g = \x -> f (g x)

f . g . h = \x -> f (g (h x))




f = negate . sum

_ = f [1, 2, 3, 4, 5]   --`15`


-}





g2' :: Int -> [Int] -> Int
g2' z xs = foldr (+) z xs

g2'' = foldr (+)
_ = g2'' 0 [1..5]



g2''' = length . filter (== 'a')
_ = g2''' "abracadabra"




-- Polymorphic constant: A concrete data constructor that stays polymorphic
-- until given a more specific type. It will have to resolve to a concrete
-- type in order to evaluate.


{- GHCi ------------------------------------------------------------------------

> :t 10
10 :: Num p => p

> :t (10 + 10)
(10 + 10) :: Num a => a

> :t 9.8
9.8 :: Fractional p => p

> :t (10 + 9.8)
(10 + 9.8) :: Fractional a => a

-------------------------------------------------------------------------------}


-- The compiler can be forced to treat values as a specific type by declaring
-- the type explicitly.

n = 10 :: Float
-- `n` has the type `Float`.

-- A type class constraint cannot be explicitly declared.

{-
_ = n = 10 :: Fractional
_ = n = 10 :: Fractional a
_ = n = 10 :: (Fractional a)
-}




-- -----------------------------------------------------------------------------

-- `flip` will exchange the order of a two-parameter function's parameters.

_ = (-) 10 1         -- `9`
_ = flip (-) 10 1    -- `-9`

{-
_ = foldl (:) [] [1, 2, 3]    -- Error.
-}
_ = foldl (flip (:)) [] [1, 2, 3]    -- `[3, 2, 1]`
-- ((([] (flip (:)) 1) (flip (:)) 2) (flip (:)) 3)
-- (([1] (flip (:)) 2) (flip (:)) 3)
-- ([2, 1] (flip (:)) 3)
-- [3, 2, 1]

-- -----------------------------------------------------------------------------





returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

{-
returnBroke :: (((a -> b) -> c) -> d) -> d
returnBroke _ _ _ d = d
-}

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a






{-

"boobs" operator
bird combinator

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)


-}












-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------





{-



-- From:
-- http://sakshamsharma.com/2018/03/haskell-proj-struct/





-- Imports example:

import MyFile                -- Import all exported items in this module
                             -- or
import MyFile (fancyFxn)     -- Only import fancyFxn
                             -- or
import qualified MyFile as M -- Imports fancyFxn as M.fancyFxn and so on







-- Exports example:

module MyExports
  ( SomeTypeWithoutItsFxns
  , SomeOtherType(..)
  , something
  , module MyMinorExports
  , MyMajorExports.SomeType(..)
  , MyMinorExports.fxnToHandleType
  ) where

import MyMinorExports
import MyMajorExports

data SomeTypeWithoutItsFxns =
  SomeTypeWithoutItsFxns
  { unexportedMember1 :: Int
  , unexportedMember2 :: Bool
  }

data SomeOtherType =
  SomeOtherType
  { member1 :: Int
  , member2 :: Bool
  }

something :: Int -> Bool -> SomeTypeWithoutItsFxns
something i b =
  SomeTypeWithoutItsFxns
  { unexportedMember1 = i
  , unexportedMember2 = b
  }



-}





-- "The Not-A-Wat in Haskell"
-- https://youtu.be/87re_yIQMDw

_ = length (1, 2)            -- `1`
_ = length Nothing           -- `0`
_ = length $ Just Nothing    -- `0`
_ = length $ Just ()         -- `1`

_ = foldr (+) 0 (3, 99)    -- `99`

_ = fmap show (1, 2)          -- `(1, "2")`
_ = fmap show (False, 2)      -- `(False, "2")`
_ = fmap show (True, True)    -- `(True, "True")`

{-
_ = foldr (+) 0 (3, "99")    -- Error.
_ = foldr (+) 0 (1, 2, 3)    -- Error.
-}










-- A constructor with an alphanumeric name is prefix by default.

-- A constructor with a non-alphanumeric name is infix by default.





-- A function with an alphanumeric name is prefix by default.

-- A function with a non-alphanumeric name is infix by default.











-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------


{-


f = (+ 2)
f' x = (+ 2) x



\x -> outside (inside x)
\x -> outside $ inside x
\x -> outside . inside $ x
      outside . inside

`outside` composed with `inside`







-- eta abstraction example
-- [1]: https://youtu.be/seVSlKazsNk?t=6m33s
aggregate f    = sum . map f
aggregate f xs = sum . map f $ xs
aggregate f xs = sum $ map f xs
aggregate f xs = sum (map f xs)

-- eta reduction example
-- [1]: https://youtu.be/seVSlKazsNk?t=7m11s
aggregate f =           sum . map f
aggregate f =       (.) sum  (map f)
aggregate f =       (.) sum $ map f
aggregate f =       (.) sum . map $ f
aggregate   = \f -> (.) sum . map $ f
aggregate f =       (.) sum . map
aggregate f =          (sum .) . map



aggregate f =     ((.) . (.)) sum map





-- [1]: https://youtu.be/seVSlKazsNk?t=20m2s
\x y z -> _
\x -> (\y -> (\z -> _))




-}






functionH :: [a] -> a
functionH (x:_) = x


functionC :: Ord a => a -> a -> Bool
functionC x y = x > y


functionS :: (a, b) -> b
functionS (_, y) = y





myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = undefined














-- data
-- type
-- newtype





















-- from "Haskell 101"
-- https://youtu.be/cTN1Qar4HSw?t=2097

-- f       :: Int -> (Int -> [Int])
-- f       ::   Int -> Int -> [Int]
-- f 1     ::          Int -> [Int]
-- f 1 2   ::                 [Int]
-- (f 1) 2 ::                 [Int]






-- from "Haskell 101"
-- https://youtu.be/cTN1Qar4HSw?t=2729

--          show :: Stuff -> String
-- length        ::          String -> Int
-- length . show :: Stuff           -> Int














-- [1]: "Point-Free or Die: Tacit Programming in Haskell and Beyond" by Amar Shah
--      https://youtu.be/seVSlKazsNk





-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------


-- A Haskell source code file has the `.hs` extension.
-- For example: `test.hs`

-- `sayHello` has the type of `String -> IO ()`
sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")


-- main :: IO ()
-- main = putStrLn "Hello world!"


-- -}







xx' :: String
xx' = undefined
-- `xx` is a string value.

yy' :: IO String
yy' = undefined
-- `yy` is a method which produces a string value by potentially
-- performing side effects.







-- -----------------------------------------------------------------------------

-- id == \x -> const x x

_ = id 1                                                                -- `1`
_ = id id 1                                                             -- `1`
_ = id id id id id id id id id id id id id id id id id id id id id 1    -- `1`

_ = const 1 $ 9                                                    -- `1`
_ = const const const 1 $ 9                                        -- `1`
_ = const const const const const const const const const 1 $ 9    -- `1`

-- -----------------------------------------------------------------------------








data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a , psecond :: b } deriving (Eq, Show)

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.00001

myRecord' :: RecordProduct Integer Float
myRecord' = RecordProduct { pfirst = 42, psecond = 0.00001 }

-- With record syntax we can reorder field assignments without
myRecord'' :: RecordProduct Integer Float
myRecord'' = RecordProduct { psecond = 0.00001, pfirst = 42 }


newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)





type Numba = Int
a = 3 :: Int
b = 3 :: Numba

_ = a == b    -- `True`





















data OperatingSystem
  = GnuPlusLinux
  | FreeBSD
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem
  , lang :: ProgrammingLanguage
  } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, FreeBSD, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]




partialAf = Programmer { os = GnuPlusLinux }    -- Exception.
-- Triggers a warning when the `-Wmissing-fields` compiler option is used.
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#ghc-flag--Wmissing-fields











-- "Percolate values through your programs, not bottoms."
-- - Haskell Programming from First Principles, Chapter 11, page 432 of "screen" version



{-

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show

data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _                        = False

data FarmerRec = FarmerRec
  { name :: Name
  , acres :: Acres
  , farmerType :: FarmerType
  } deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _           -> False




-- `Maybe` is the preferred over records as a way to express an "empty" value.
-- A record's accessors won't work on types that wrap the record in a sum.

data Automobile = Null | Car
                          { make :: String
                          , model :: String
                          , year :: Integer
                          }
                  deriving (Eq, Show)

_ = make Null    -- Exception.

-- Split out the record/product to avoid the runtime exception and let the
-- compiler catch the error at compile time.

data Car = Car
  { make :: String
  , model :: String
  , year :: Integer
  } deriving (Eq, Show)

data Automobile = Null | Automobile Car deriving (Eq, Show)







data Quantum = Yes | No | Both deriving (Eq, Show)

-- 3 + 3
quantSum1 :: Either Quantum Quantum

-- 3 * 3
quantProd1 :: (Quantum, Quantum)

-- 3 ^ 3
quantFlip1 :: Quantum -> Quantum





-}







-- A side effect is a potentially observable result apart from the value
-- an expression evaluates to.









-- The `Maybe` datatype is used to express the possible absence of a value.

-- `Maybe'` is defined similarly to `Maybe`.

data Maybe' a = Nothing' | Just' a

-- `Nothing` represents a safe way, without hitting bottom, to express that no
-- valid value can be produced.

-- `Just a` is used to hold valid data.

f3 :: Bool -> Maybe Int
f3 False = Just 0
f3 _     = Nothing












dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

-- dividedBy 10 2

-- -- first we'll do this the previous way,
-- -- but we'll keep track of how many
-- -- times we subtracted.
-- 10 divided by 2 ==
--   10 - 2, 8 (subtracted 1 time)
--      - 2, 6 (subtracted 2 times)
--      - 2, 4 (subtracted 3 times)
--      - 2, 2 (subtracted 4 times)
--      - 2, 0 (subtracted 5 times)

-- dividedBy 10 2 =
--   go 10 2 0
--     | 10 < 2 = ...
--     -- false, skip this branch
--     | otherwise = go (10 - 2) 2 (0 + 1)
--     -- otherwise is literally the value True
--     -- so if first branch fails,
--     -- this always succeeds
--     go 8 2 1
--     -- 8 isn't < 2, so the otherwise branch
--     go (8 - 2) 2 (1 + 1)
--     -- n == 6, d == 2, count == 2
--     go 6 2 2
--     go (6 - 2) 2 (2 + 1)
--     -- 6 isn't < 2, so the otherwise branch
--     -- n == 4, d == 2, count == 3

--     go 4 2 3
--     go (4 - 2) 2 (3 + 1)
--     -- 4 isn't < 2, so the otherwise branch
--     -- n == 2, d == 2, count == 4
--     go 2 2 4
--     go (2 - 2) 2 (4 + 1)
--     -- 2 isn't < 2, so the otherwise branch
--     -- n == 0, d == 2, count == 5
--     go 0 2 5
--     -- the n < d branch is finally evaluated
--     -- because 0 < 2 is true
--     -- n == 0, d == 2, count == 5
--     | 0 < 2 = (5, 0)

-- (5, 0)


















data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y

_ = eval (Add (Lit 1) (Lit 9001))    -- `9002`





printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y

_ = printExpr (Add (Lit 1) (Lit 9001))    -- `"1 + 9001"`

a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2

_ = printExpr a3    -- `"1 + 9001 + 1 + 20001"`





-- -----------------------------------------------------------------------------

_ = 1 :| [2, 3]    -- `1 :| [2,3]`
-- it :: Num a => GHC.Base.NonEmpty a




-- -----------------------------------------------------------------------------













combo :: String -> String -> [(Char, Char, Char)]
combo stops vowels = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

_ = combo "pbt" "aeiou"    -- `[('p', 'a', 'p'), ('p', 'a', 'b'), ('p', 'a', 't'), ('p', 'e', 'p'), ('p', 'e', 'b'), ('p', 'e', 't'), ('p', 'i', 'p'), ('p', 'i', 'b'), ('p', 'i', 't'), ('p', 'o', 'p'), ('p', 'o', 'b'), ('p', 'o', 't'), ('p', 'u', 'p'), ('p', 'u', 'b'), ('p', 'u', 't'), ('b', 'a', 'p'), ('b', 'a', 'b'), ('b', 'a', 't'), ('b', 'e', 'p'), ('b', 'e', 'b'), ('b', 'e', 't'), ('b', 'i', 'p'), ('b', 'i', 'b'), ('b', 'i', 't'), ('b', 'o', 'p'), ('b', 'o', 'b'), ('b', 'o', 't'), ('b', 'u', 'p'), ('b', 'u', 'b'), ('b', 'u', 't'), ('t', 'a', 'p'), ('t', 'a', 'b'), ('t', 'a', 't'), ('t', 'e', 'p'), ('t', 'e', 'b'), ('t', 'e', 't'), ('t', 'i', 'p'), ('t', 'i', 'b'), ('t', 'i', 't'), ('t', 'o', 'p'), ('t', 'o', 'b'), ('t', 'o', 't'), ('t', 'u', 'p'), ('t', 'u', 'b'), ('t', 'u', 't')]`


combo' :: String -> String -> [(Char, Char, Char)]
combo' stops vowels = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

_ = combo' "pbt" "aeiou"    -- `[('p', 'a', 'p'), ('p', 'a', 'b'), ('p', 'a', 't'), ('p', 'e', 'p'), ('p', 'e', 'b'), ('p', 'e', 't'), ('p', 'i', 'p'), ('p', 'i', 'b'), ('p', 'i', 't'), ('p', 'o', 'p'), ('p', 'o', 'b'), ('p', 'o', 't'), ('p', 'u', 'p'), ('p', 'u', 'b'), ('p', 'u', 't')]`


nouns = ["pap", "pat", "pad", "peg"]
verbs = ["pit", "tab", "tag", "dab"]

combo'' :: String -> String -> [String] -> [String] -> [(String, String, String)]
combo'' stops vowels nouns verbs =
  [ (x, y, z)
  | x <- words, y <- words, z <- words
  , elem x nouns, elem y verbs, elem z nouns
  ]
    where words = [[x, y, z] | x <- stops, y <- vowels, z <- stops]

_ = combo'' "pbt" "aeiou" nouns verbs    -- `[("pap", "pit", "pap"), ("pap", "pit", "pat"), ("pap", "tab", "pap"), ("pap", "tab", "pat"), ("pat", "pit", "pap"), ("pat", "pit", "pat"), ("pat", "tab", "pap"), ("pat", "tab", "pat")]`







-- -----------------------------------------------------------------------------

h2 = undefined
g2 = undefined

f6 x y z = h2 (subFunction x y z)
  where subFunction x y z = g2 x y z

-- The above is not tail recursive, calls `h2`, not itself.
f6 x y z = h2 (f6 (x - 1) y z)

-- Still not tail recursive. `f6` is invoked again but not in the tail call of
-- `f6`; it's an argument to the tail call, `h2`:
f6 x y z = f6 (x - 1) y z

-- This is tail recursive. `f6` is calling itself directly with no intermediaries.

-- To avoid naming conflicts, well use the name `myFoldr` instead of `foldr`.
myFoldr f z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

-- Not tail recursive, we give up control to the combining function `f` before
-- continuing through the list. `foldr`'s recursive calls will bounce between
-- `foldr` and `f`.

-- To avoid naming conflicts, well use the name `myFoldl` instead of `foldl`.
myFoldl f z []     = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

-- Tail recursive. `foldl` invokes itself recursively. The combining function is
-- only an argument to the recursive fold.








-- -----------------------------------------------------------------------------


-- Error handling.


ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing




-- A "smart constructor" creates only values that meet a certain criteria and
-- signals when it can't.









-- Pattern matching is a case expression, where the data constructor is the
-- condition. Case expressions and pattern matching will work without an
-- `Eq` instance, but guards using `==` will not.








-- Type constructors (that is, higher-kinded types) are types that take more
-- types as arguments.

-- The Haskell Report uses the term "type constant" to refer to types that take
-- no arguments and are already types.

-- In the Report, type constructor is used to refer to types which must have
-- arguments applied to become a type.



{- GHCi ------------------------------------------------------------------------

> :kind Int
Int :: *

> :k Bool
Bool :: *

> :k Char
Char :: *

-------------------------------------------------------------------------------}






-- We use Left as our invalid or error constructor for a couple of reasons. It
-- is conventional to do so in Haskell, but that convention came about for a
-- reason. The reason has to do with the ordering of type arguments and
-- application of functions. Normally it is your error or invalid
-- result that is going to cause a stop to whatever work is being
-- done by your program. Functor will not map over the left type
-- argument because it has been applied away. Since you normally
-- want to apply functions and map over the case that doesn't
-- stop your program (that is, not the error case), it has
-- become convention that the Left of Either is used for
-- whatever case is going to cause the work to stop.











data Example a = Blah | RoofGoats | Woot a

{- GHCi ------------------------------------------------------------------------

> :k Example
Example :: * -> *

-------------------------------------------------------------------------------}











{- GHCi ------------------------------------------------------------------------

> :k (,)
(,) :: * -> * -> *

> :k (Int, Int)
(Int, Int) :: *

-------------------------------------------------------------------------------}




{- GHCi ------------------------------------------------------------------------

> :k Maybe
Maybe :: * -> *

> :k Maybe Int
Maybe Int :: *

> :k Either
Either :: * -> * -> *

> :k Either Int
Either Int :: * -> *

> :k Either Int String
Either Int String :: *

> :k Maybe Maybe
<interactive>:1:7-11: error:
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’
    • In the first argument of ‘Maybe’, namely ‘Maybe’
      In the type ‘Maybe Maybe’

> :k Maybe (Maybe Int)
Maybe (Maybe Int) :: *

-------------------------------------------------------------------------------}



{- GHCi ------------------------------------------------------------------------

> :k []
[] :: * -> *

> :k [] Int
[] Int :: *

> :k [Int]
[Int] :: *

> :k Maybe []
<interactive>:1:7-8: error:
    • Expecting one more argument to ‘[]’
      Expected a type, but ‘[]’ has kind ‘* -> *’
    • In the first argument of ‘Maybe’, namely ‘[]’
      In the type ‘Maybe []’

> :k Maybe [Bool]
Maybe [Bool] :: *

-------------------------------------------------------------------------------}










-- The kind `*` is the kind of all standard lifted types, while types that have
-- the kind `#` are unlifted.

-- A lifted type, which includes any datatype you could define yourself, is any
-- that can be inhabited by "bottom".

-- Unlifted types are any type which cannot be inhabited by bottom.

-- Types of kind `#` are often native machine types and raw pointers.

-- Newtypes are a special case in that they are kind `*`, but are unlifted
-- because their representation is identical to that of the type they
-- contain, so the newtype itself is not creating any new pointer
-- beyond that of the type it contains.







{-



a = [] :: [Int]
a = [] :: [] Int


_ = [1..10 :: Int]
_ = [1..10] :: [] Int
_ = [1..10] :: [Int]


-}



{- GHCi ------------------------------------------------------------------------

> data Trivial = Trivial

> :k Trivial
Trivial :: *

> Trivial 1
<interactive>:24:1-9: error:
    • Couldn't match expected type ‘Integer -> t’
                  with actual type ‘Trivial’
    • The function ‘Trivial’ is applied to one argument,
      but its type ‘Trivial’ has none
      In the expression: Trivial 1
      In an equation for ‘it’: it = Trivial 1
    • Relevant bindings include it :: t (bound at <interactive>:24:1)

> data Unary a = Unary a

> :k Unary
Unary :: * -> *

> data Unary = Unary a deriving Show
<interactive>:25:20: error: Not in scope: type variable ‘a’

> data TwoArgs a b = TwoArgs a b

> :k TwoArgs
TwoArgs :: * -> * -> *

> data ThreeArgs a b c = ThreeArgs a b c

> :k ThreeArgs
ThreeArgs :: * -> * -> * -> *

-------------------------------------------------------------------------------}





_ = fmap Just [1, 2, 3]    -- `[Just 1, Just 2, Just 3]`






-- -----------------------------------------------------------------------------









notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x

_ = notThe "the"            -- `Nothing`
_ = notThe "blahtheblah"    -- `Just "blahtheblah"`
_ = notThe "woot"           -- `Just "woot"`



{-

import Data.Maybe

replaceThe :: String -> String
replaceThe (x:y:z:xs) = fromMaybe "a" (notThe $ x:y:z:[]) ++ replaceThe xs
replaceThe xs         = xs

_ = replaceThe "the cow loves us"    -- `"a cow loves us"`

-}








countTheBeforeVowel :: String -> Integer
countTheBeforeVowel (v:w:x:y:z:xs) =
  if (v:w:x:y:[]) == "the " && elem z "aeiouy"
  then (1 + countTheBeforeVowel xs)
  else countTheBeforeVowel (w:x:y:z:xs)
countTheBeforeVowel xs = 0

_ = countTheBeforeVowel "the cow"         -- `0`
_ = countTheBeforeVowel "the evil cow"    -- `1`









countVowels :: String -> Integer
countVowels = foldr (\x acc -> if elem x "aeiouy" then (acc + 1) else acc) 0

_ = countVowels "the cow"        -- `2`
_ = countVowels "Mikolajczak"    -- `4`






{-

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord xs
  | vowelCount > consonantCount = Nothing
  | otherwise = Just $ Word' xs
  where vowelCount     = length $ filter (\letter -> elem letter vowels) xs
        consonantCount = length $ filter (\letter -> notElem letter vowels) xs

-}














data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

_ = natToInteger Zero                  -- `0`
_ = natToInteger (Succ Zero)           -- `1`
_ = natToInteger (Succ (Succ Zero))    -- `2`

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0     = Nothing
  | otherwise = Just $ snd $ go (x, Zero)
  where go (0, nat) = (0, nat)
        go (n, nat) = go (n - 1, Succ nat)

_ = integerToNat 0       -- `Just Zero`
_ = integerToNat 1       -- `Just (Succ Zero)`
_ = integerToNat 2       -- `Just (Succ (Succ Zero))`
_ = integerToNat (-1)    -- `Nothing`










mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z _ Nothing  = z
mayybee _ f (Just x) = f x

_ = mayybee 0 (+1) Nothing     -- `0`
_ = mayybee 0 (+1) (Just 1)    -- `2`




fromMaybe' :: a -> Maybe a -> a
fromMaybe' x = mayybee x id

_ = fromMaybe' 0 Nothing     -- `0`
_ = fromMaybe' 0 (Just 1)    -- `1`







fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe x (Just y) = y

_ = fromMaybe 0 Nothing     -- `0`
_ = fromMaybe 0 (Just 1)    -- `1`













listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just $ head xs

_ = listToMaybe [1, 2, 3]    -- `Just 1`
_ = listToMaybe []           -- `Nothing`






maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

_ = maybeToList (Just 1)    -- `[1]`
_ = maybeToList Nothing     -- `[]`





catMaybes :: [Maybe a] -> [a]
catMaybes []            = []
catMaybes (Nothing:xs)  = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs

_ = catMaybes [Just 1, Nothing, Just 2]    -- `[1,2]`
_ = catMaybes $ take 3 $ repeat Nothing    -- `[]`








flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe []            = Just []
flipMaybe (Nothing:xs)  = Nothing
flipMaybe ((Just x):xs) =
  case flipMaybe xs of
    Nothing -> Nothing
    Just xs -> Just (x:xs)

_ = flipMaybe [Just 1, Just 2, Just 3]     -- `Just [1, 2, 3]`
_ = flipMaybe [Just 1, Nothing, Just 3]    -- `Nothing`












lefts' :: [Either a b] -> [a]
lefts' = foldr go []
  where go (Left x)  acc = x : acc
        go (Right _) acc = acc

_ = lefts' [Left 1, Right 2, Left 3]    -- `[1,3]`



rights' :: [Either a b] -> [b]
rights' = foldr go []
  where go (Left _)  acc = acc
        go (Right x) acc = x : acc

_ = rights' [Left 1, Right 2, Left 3]    -- `[2]`





partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)




eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right x) = Just $ f x




either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ f (Right x) = f x


{-


eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' = either' (const Nothing)


-}

{- GHCi ------------------------------------------------------------------------

-- iterate is like a limited
-- unfold that never ends
> :t iterate
iterate :: (a -> a) -> a -> [a]

-------------------------------------------------------------------------------}




-- because it never ends, we must use
-- take to get a finite list
_ = take 10 $ iterate (+1) 0    -- `[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]`



{-

-- unfoldr is more general
> :t unfoldr
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]





import Data.List

-- Using unfoldr to do
-- the same thing as iterate
_ = take 10 $ unfoldr (\b -> Just (b, b + 1)) 0    -- `[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]`



-}








myIterate :: (a -> a) -> a -> [a]
myIterate f z = z : (myIterate f $ f z)

_ = take 10 $ myIterate (+1) 0    -- `[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]`


myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f z = go $ f z
  where go Nothing = []
        go (Just (x, y)) = x : myUnfoldr f y


betterIterate :: (a -> a) -> a -> [a]
betterIterate f z = myUnfoldr (\x -> Just (x, f x)) z
-- betterIterate f = myUnfoldr (\x -> Just (x, f x))
-- betterIterate f = myUnfoldr $ \x -> Just (x, f x)

_ = take 10 $ betterIterate (+1) 0    -- `[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]`













data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f z = go $ f z
  where go Nothing = Leaf
        go (Just (x, y, z)) = Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x < n then Just (x + 1, x, x + 1) else Nothing) 0

_ = treeBuild 0    -- `Leaf`
_ = treeBuild 1    -- `Node Leaf 0 Leaf`
_ = treeBuild 2    -- `Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)`
_ = treeBuild 3    -- `Node (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf)) 0 (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf))`











-- -----------------------------------------------------------------------------

-- Typed holes are useful for letting the compiler help us to fill in missing
-- parts of our program.

-- A hole is denoted by an underscore, `_`, on the term level.

-- Holes will cause a compilation error but will also make the compiler list
-- relevant variables in scope that could be used to fill in the hole.

{-
xx'' :: Int
xx'' = _    -- Error.
-}

-- A related concept, the type wildcard, also denoted by `_`, is enabled by using
-- the PartialTypeSignatures GHC extension.

{-
xx'' :: _
xx'' = 2
-}

-- -----------------------------------------------------------------------------














-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------


-- Haskell programs are organized into modules. Modules contain the datatypes,
-- type synonyms, type classes, type class instances, and values you've
-- defined at the top level. They offer a means to import other modules
-- into the scope of your program, and they also contain values that
-- can be exported to other modules.






-- The Haskell Cabal, or Cabal, or the Common Architecture for Building
-- Applications and Libraries, is a package manager for Haskell.





-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------








{- GHCi ------------------------------------------------------------------------

> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c

> :t (.) . (.)
(.) . (.) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c

> :t (.) . (.) . (.)
(.) . (.) . (.) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c

-------------------------------------------------------------------------------}





-- -----------------------------------------------------------------------------




(.:) = (.) . (.)

-- http://sleepomeno.github.io/blog/2014/08/14/Composing-two-argument-functions/
-- f''' = return .: prompt
-- f''' = (return .) . prompt
-- f''' = \modules lineNumber -> return $ prompt modules lineNumber


-- -----------------------------------------------------------------------------




-- Each argument (and result) in the type signature for a function must be a
-- fully applied type. Each argument must have the kind `*`.

-- Each argument and result of every function must be a type constant, not a
-- type constructor.






-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------



{-

Key Terms
=========
Top-level binding: A binding scoped to a module and not nested.
Top-level declaration: A top-level binding.
Module: A group of top-level declarations that may be exported elsewhere.
Local binding: A binding nested and scoped within another expression.
Local declaration: A local binding.

Data structure: An organizing of data for convenient and/or efficient access.

Arity: The number of arguments a function accepts.


Curried function: A nested set of single-parameter functions.
Uncurried function: In Haskell, a function taking a tuple of many arguments.


Bottom: An indication that a computation does not result in a value.


Directive: An instruction to the compiler.


Predicate: A function that evaluates to a boolean value.


Side effect: A result apart from the value of an expression.



Idempotence: A function property where result is fixed after one application.



Hole:


Language extension: A compiler feature that goes beyond the Haskell standard.

Pragma: A special compiler instruction embedded within source code.



Package: A program including all of its modules and dependencies.

Dependency: An interlinked element of a program like libraries and tests.
Cabal: Common Architecture for Building Applications and Libraries
The Haskell Cabal (Cabal): A package manager for Haskell.
Stack: A project manager for Haskell.

Haskeller: A Haskell programmer, especially one who is proficient.




Unit testing: A method in which the smallest parts of an application are tested.
Property testing: A method where a subset of an input space is validated.

-}
