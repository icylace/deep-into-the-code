



-- The function `foo`
foo x =
  let y = x * 2
      z = x ^ 2
  in 2 * y * z













-- Key Terms
-- =========
-- Currying: Applying a series of nested functions to a series of arguments.
-- Partial application: The fixing of arguments to a function to produce
--     another function that takes the remaining unfixed paramters.
-- Syntactic sugar: Syntax designed to make expressions easier to work with.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------













--  Type - The kind of data that can be processed.





-- A Haskell source code file has the `.hs` extension.
-- For example: `test.hs`

-- `sayHello` has the type of `String -> IO ()`
sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")





main :: IO ()
main = putStrLn "Hello World"
