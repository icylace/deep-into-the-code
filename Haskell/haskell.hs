-- This is a single-line comment.

-- Comments let you put helpful commentary with your code.

--Not having a space leading the comment text works but looks awkward.

{- Nested comments are
usually seen spanning
multiple lines. -}

{- Nested comments can be used like single-line comments. -}

{-Not having spaces surrounding nested comment text is works but is awkward.-}

{- Nested comments can be -}  {- placed next to each other. -}

{- Not that you
would -}{- want
to do that. -}

{- Also, nested comments...
  {- ...may be nested, of course. -}
-}

      -- Single-line comments don't need to start at the beginning of the line.

      {- Neither do nested comments. -}

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- This file you're reading right now is a Haskell source code file because it
-- has the `.hs` file extension and contains Haskell code.

import O01__Expressions
import O02__Functions
import O03__Operators
import O04__Arithmetic
import O05__Whitespace
import O06__LetAndWhere
import O07__Strings
import O08__Types
import O09__Branching
import O10__Tuples
import O11__PatternMatching
import O12__Lists
import O13__Typeclasses
import O14__Equality
import O15__Ordering
import O16__Enumeration
import O17__Showing
import O18__Reading
import O19__Recursion
import O20__Folding
import O21__AlgebraicDatatypes

-- Any Haskell source code file that is meant to be run directly must have the
-- entry point `main` defined.  `main` will be explained in more detail later.

main :: IO ()
main = print ()

-- To run the code in this file, the source code must first be compiled into an
-- executable binary file before being run.  Executing the following on the
-- command line:
--
--     stack runghc haskell
--
-- If there were any compile-time errors while attempting to run, GHC will
-- show them.

-- Another way is to load the code from within the GHCi REPL environment.  While
-- remaining in the same directory, open up GHCi by running
--
--     stack ghci
--
-- then use GHCi's load command, `:l` (short for `:load`):
--
--     :l haskell
--
-- If there were any compile-time errors while loading, GHCi will show them.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========
-- Comment: Text meant to be read by humans and ignored by the compiler.
-- Single-line comment: A comment that occupies one line.
-- Block comment: A comment that occupies one or more consecutive lines.
-- Nested comment: A block comment that can contain other block comments.

-- Entry point: Place where the operating system transfers control to a program.
















-- TODO:





-- The arrow, `->`, is the function type constructor.  It's like other type
-- constructors except that it takes arguments while having no
-- data constructors.

-- The values of the function type are functions.

-- `->` is also a right-associative infix operator.

-- In contrast, function application is left-associative.








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
-- happen for code yet to be implemented.  Evaluating "undefined" code
-- will throw an exception.  Thanks to lazy evaluation, such code can
-- coexist with normal.

x = undefined
-- `x` is now undefined.

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

_ = fst (2, 3)    -- `2`
_ = first 2 3     -- `2`
_ = (first 2) 3   -- `2`







-- `uncurry` unnests a nested function.

addUsingPair = uncurry (+)

_ = 2 + 3                 -- `5`
_ = (+) 2 3               -- `5`
_ = addUsingPair (2, 3)   -- `5`









curriedStyle x y      = x + y
uncurriedStyle (x, y) = x + y
anonymousStyle        = \x y -> x + y
nestedAnonymousStyle  = \x -> \y -> x + y

_ = curriedStyle 2 3            -- `5`
_ = uncurriedStyle (2, 3)       -- `5`
_ = anonymousStyle 2 3          -- `5`
_ = nestedAnonymousStyle 2 3    -- `5`












f :: a -> a -> a -> a
f = undefined

y :: Char
y = undefined

-- The type of `f x` is `Char -> Char -> Char`.


g :: a -> b -> c -> b
g = undefined

_ = g 0 'c' "woot"
-- Type is `Char`.

h :: (Num a, Num b) => a -> b -> b
h = undefined

_ = h 1.0 2
-- Type is `Num b => b`.

_ = h 1 (5.5 :: Double)
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
addOnePF = (+ 1)

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

-- The following is an example of WHNF evaluation.  `myNum` is in WHNF because
-- it's a list constructed by a range that will only evaluate as far as it has
-- to.  `take 2` forces some evaluation of the range. `:sprint` displays what
-- has been evaluated (the numbers) and what hasn't (the underscore).

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









-- The fundamental way to think about evaluation in Haskell is as substitution.









-- {-




{-


_ = jackal "keyboard"
-- Type is `Eq b => b -> [Char]`.


-}



kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined

_ = kessel 1 2
-- Type is `(Num a, Ord a) => a`.

_ = kessel 1 (2 :: Integer)
-- Type is `(Num a, Ord a) => a`.

_ = kessel (1 :: Integer) 2
-- Type is `Integer`.













-- The composition operator, `.`, takes a couple functions and composes them to
-- make a new function.

-- Prelude> :i (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c 	-- Defined in ‘GHC.Base’
-- infixr 9 .

g1 = negate . sum

_ = g1 [1, 2, 3, 4, 5]    -- `-15`

{-

negate . sum $ [1, 2, 3, 4, 5]

negate (sum [1, 2, 3, 4, 5])

(negate . sum) [1, 2, 3, 4, 5]

-}



_ = take 5 . filter odd . enumFrom $ 3    -- `[3,5,7,9,11]`





{-

-- https://stackoverflow.com/a/13147064
-- https://stackoverflow.com/a/2465059
-- https://stackoverflow.com/a/1983310

f x y
(f x) y
(x `f`) y
(`f` y) x
x `f` y

f $ g x y
f . g $ x y
f . g x $ y

f $ g $ h x
(f . g . h) x
f . g . h $ x

f . g . h
(f . g) . h
f . (g . h)









(f . g) x = f (g x)

f . g = \x -> f (g x)

f . g . h = \x -> f (g (h x))




f = negate . sum

_ = f [1, 2, 3, 4, 5]   --`15`


-}





g2 :: Int -> [Int] -> Int
g2 z xs = foldr (+) z xs

g2' = foldr (+)
_ = g2' 0 [1..5]



g2'' = length . filter (== 'a')
_ = g2'' "abracadabra"




-- Polymorphic constants stay polymorphic until given a more specific type.  It
-- will have to resolve to a concrete type in order to evaluate.

-- :t 10
-- 10 :: Num p => p

-- :t (10 + 10)
-- (10 + 10) :: Num a => a

-- :t 9.8
-- 9.8 :: Fractional p => p

-- :t (10 + 9.8)
-- (10 + 9.8) :: Fractional a => a

-- The compiler can be forced to treat values as a specific type by declaring
-- the type explicitly.

n = 10 :: Float
-- `n` has the type `Float`.

-- A typeclass constraint cannot be explicitly declared.

{-
_ = n = 10 :: Fractional
_ = n = 10 :: Fractional a
_ = n = 10 :: (Fractional a)
-}




-- -----------------------------------------------------------------------------

-- `flip` will exchange the order of a two-parameter function's parameters.

_ = (-) 10 1        -- `9`
_ = flip (-) 10 1   -- `-9`

{-
_ = foldl (:) [] [1, 2, 3]    -- Results in a compiler error.
-}
_ = foldl (flip (:)) [] [1, 2, 3]   -- `[3,2,1]`
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
aggregate f =       (.) sum (map f)
aggregate f =       (.) sum $ map f
aggregate f =       (.) sum . map $ f
aggregate   = \f -> (.) sum . map $ f
aggregate f =       (.) sum . map
aggregate f =          (sum .) . map






-- [1]: https://youtu.be/seVSlKazsNk?t=20m2s
\x y z -> _
\x -> (\y -> (\z -> _))




-}






functionH :: [a] -> a
functionH (x:_) = x


functionC :: Ord a => a -> a -> Bool
functionC x y = x > y


functionS :: (a, b) -> b
functionS (x, y) = y





myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = undefined




















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







xx :: String
xx = undefined
-- `xx` is a string value.

yy :: IO String
yy = undefined
-- `yy` is a method which produces a string value by potentially
-- performing side effects.












-- A side effect is a potentially observable result apart from the value
-- an expression evaluates to.










-- The `Maybe` datatype is used to express the possible absence of a value.

-- `Maybe'` is defined similarly to `Maybe`.

data Maybe' a = Nothing' | Just' a

-- The `Nothing` value represents a safe way, without hitting bottom, to express
-- that no valid value can be produced.

-- The `Just a` value is used to hold valid data.

f3 :: Bool -> Maybe Int
f3 False = Just 0
f3 _     = Nothing












dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
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







-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------



-- Key Terms
-- =========
-- Top-level binding: A binding scoped to a module and not nested.
-- Top-level declaration: A top-level binding.
-- Module: A group of top-level declarations that may be exported elsewhere.
-- Local binding: A binding nested and scoped within another expression.
-- Local declaration: A local binding.

-- Data structure: An organizing of data for convenient and/or efficient access.

-- Arity: The number of arguments a function accepts.


-- Curried function: A nested set of single-parameter functions.
-- Uncurried function: In Haskell, a function taking a tuple of many arguments.


-- Bottom: An indication that a computation does not result in a value.



-- Directive: An instruction to the compiler.



-- Predicate: A function that evaluates to a boolean value.



-- Side effect: A result apart from the value of an expression.



-- Hole:

