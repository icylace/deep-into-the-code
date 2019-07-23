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

import O00__unsorted
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
import O13__TypeClasses
import O14__Equality
import O15__Ordering
import O16__Enumeration
import O17__Showing
import O18__Reading
import O19__Recursion
import O20__Folding
import O21__AlgebraicDatatypes
import O22__SemigroupAndMonoid
import O23__Functor

-- Any Haskell source code file that is meant to be run directly must have the
-- entry point `main` defined. `main` will be explained in more detail later.

main :: IO ()
main = print ()

-- To run the code in this file, the source code must first be compiled into an
-- executable binary file before being run. Executing the following on the
-- command line:
--
-- ```shell
-- $ stack runghc haskell
-- ```
--
-- If there were any compile-time errors while attempting to run, GHC will
-- show them.

-- Another way is to load the code from within the GHCi REPL environment. While
-- remaining in the same directory, open up GHCi by running
--
-- ```shell
-- $ stack ghci
-- ```
--
-- then use GHCi's load command, `:l` (short for `:load`):
--
-- ```shell
-- > :l haskell
-- ```
--
-- If there were any compile-time errors while loading, GHCi will show them.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========
Comment: Text meant to be read by humans and ignored by the compiler.
Single-line comment: A comment that occupies one line.
Block comment: A comment that occupies one or more consecutive lines.
Nested comment: A block comment that can contain other block comments.

Entry point: Place where the operating system transfers control to a program.

-}

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Credits:
-- Some of my notes are copies of content from external sources, mainly
-- [Haskell Programming from First Principles](http://haskellbook.com/).
