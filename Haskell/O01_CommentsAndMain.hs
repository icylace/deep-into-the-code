-- This is a single-line comment.

-- Comments let you put helpful commentary with your code.

{- Block comments are
usually seen spanning
multiple lines. -}

{- Block comments can be used like single-line comments. -}

{- Block comments can be -}  {- placed next to each other. -}

{- Not that you
would -}{- want
to do that. -}

{- Also, block comments...
  {- ...may be nested. -}
-}

      -- Single-line comments don't need to start at the beginning of the line.

      {- Neither do block comments. -}

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

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
--     stack runghc 001-CommentsAndMain
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
--     :l 001-CommentsAndMain
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
