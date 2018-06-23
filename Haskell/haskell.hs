







-- TODO:







-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------


-- Key Terms
-- =========
-- Module: A group of top-level declarations that may be exported elsewhere.
-- Top-level binding: A binding scoped to a module and not nested.
-- Top-level declaration: A top-level binding.
-- Local binding: A binding nested and scoped within another expression.
-- Local declaration: A local binding.

-- Data structure: An organizing of data for convenient and/or efficient access.














-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------



f = (+ 2)
f' x = (+ 2) x






-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------


-- A Haskell source code file has the `.hs` extension.
-- For example: `test.hs`

-- `sayHello` has the type of `String -> IO ()`
sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")





main :: IO ()
main = putStrLn "Hello world!"
