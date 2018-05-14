-- TODO:







-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------













-- Type - The kind of data that can be processed.



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
