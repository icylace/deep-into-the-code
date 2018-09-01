-- This is a single-line comment.

-- Comments let you put helpful commentary with your code.

{- Nested comments are
usually seen spanning
multiple lines. -}

{- Nested comments can be used like single-line comments. -}

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

import O01_Expressions
import O02_Functions
import O03_Operators
import O04_Arithmetic
import O05_SignificantWhitespace
import O06_LetAndWhere
import O07_Strings
import O08_Types
import O09_Comparison
import O10_Tuples
import O11_PatternMatching
import O12_Lists
import O13_Typeclasses

-- This file you're reading right now is a Haskell source code file because
-- it has the `.hs` file extension and contains Haskell code.

-- Any Haskell source code file that is meant to be run directly must have the
-- entry point `main` defined.  `main` will be explained in more detail later.

main :: IO ()
main = print ()

-- To run the code in this file, the source code must first be compiled into
-- an executable binary file before being run.  Executing the following on
-- thecommand line:
--
--     stack runghc haskell
--
-- If there were any compile-time errors while attempting to run, GHC will
-- show them.

-- Another way is to load the code from within the GHCi REPL environment.
-- While remaining in the same directory, open up GHCi by running
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

-- Entry point: Place where the operating system transfers control to a program.
















-- TODO:





-- The arrow, `->`, is the function type constructor.  It's like other
-- type constructors except that it takes arguments while having no
-- data constructors.

-- The values of the function type are functions.

-- `->` is also a right-associative infix operator.

-- In contrast, function application is left-associative.








-- A module is a grouping of code on the file level.

-- The group consists of top-level declarations that may be exported to any
-- other part of the program that exist in other source code files.







-- The compiler gives the least specific and most general type it can.







-- `undefined` is a special value that can be used to allow compilation
-- to happen for code that is not yet implemented.  If "undefined" code
-- is evaluated, an exception will be thrown.  However, due to lazy
-- evaluation, such code can coexist with other code that works
-- and operates normally.

x = undefined
-- `x` is now undefined.

-- "undefined" code can be typechecked.







-- Since `->` is right-associative, currying is available by default,
-- and function type signatures are implicitly parenthesized.

f1 :: a -> a -> a
f1 = undefined

f2 :: a -> (a -> a)
f2 = undefined

-- `f1` and `f2` have the same types.













-- The `curry` function nests an unnested function.

first = curry fst

_ = fst (2, 3)
_ = first 2 3
_ = (first 2) 3
-- All results are `2`.







-- The `uncurry` function unnests a nested function.

addUsingPair = uncurry (+)

_ = 2 + 3
_ = (+) 2 3
_ = addUsingPair (2, 3)
-- All results are `5`.









curriedStyle x y = x + y
uncurriedStyle (x, y) = x + y
anonymousStyle = \x y -> x + y
nestedAnonymousStyle = \x -> \y -> x + y

_ = curriedStyle 2 3
_ = uncurriedStyle (2, 3)
_ = anonymousStyle 2 3
_ = nestedAnonymousStyle 2 3
-- All results are `5`.












f :: a -> a -> a -> a
f = undefined

y :: Char
y = undefined

-- The type of `f x` is `Char -> Char -> Char`


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

{-

_ = jackal "keyboard"
-- Type is `Eq b => b -> [Char]`.


kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined

_ = kessel 1 2
-- Type is `(Num a, Ord a) => a`.

_ = kessel 1 (2 :: Integer)
-- Type is `(Num a, Ord a) => a`.

_ = kessel (1 :: Integer) 2
-- Type is `Integer`.










-- Polymorphic constants stay polymorphic until given a more specific type.
-- It will have to resolve to a concrete type in order to evaluate.

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
--
--     n = 10 :: Fractional
--     n = 10 :: Fractional a
--     n = 10 :: (Fractional a)
--















-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------






-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------




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






-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------



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
aggregate f = \f -> (.) sum . map $ f
aggregate f =       (.) sum . map
aggregate f =          (sum .) . map






-- [1]: https://youtu.be/seVSlKazsNk?t=20m2s
\x y z -> _
\x -> (\y -> (\z -> _))












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


-}















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

-- Higher-order function: A function that accepts and/or returns a function.




-- Directive: An instruction to the compiler.

-- Typeclass deriving: The automatic creation of instances for new datatypes.




