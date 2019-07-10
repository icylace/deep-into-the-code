-- A module can export all top-level declarations, including datatypes.
module Hello8 where

data FooString = FooString String deriving (Show)

hello8 :: String
hello8 = "hello 8"

goodbye8 :: String
goodbye8 = "goodbye 8"
