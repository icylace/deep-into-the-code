-- TODO: add more declarations for testing purposes

-- Exports specific top-level declarations.

module Foo3 (sayHello) where

sayHello :: IO ()
sayHello = putStrLn "hello world"
