-- The application operator, `$`, lets us apply functions to arguments with
-- potentially fewer parentheses than normal function application.
_ = 2 * (3 + 5)
_ = (2 *) (3 + 5)
_ = (2 *) $ (3 + 5)
_ = (2 *) $ 3 + 5
-- All four results will be `16`.







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
