-- The module name and the name of the file that contains it must match.
-- Here, the module name is `Modules` and the filename is "Modules.hs".
module Modules where

-- By default an import brings into scope all exports from a given module.
import Hello1

-- Importing a module that has no exports does nothing.
import Hello2

-- A qualified import requires the module name be prefixed to declarations when
-- they are used.
import qualified Hello3

-- A qualified import can be given an alias.
import qualified Hello3 as H3

-- We can import only specific declarations if we choose to.
import Hello4 (goodbye4)

import Hello5
import Hello5 (hello5)

-- TODO:
-- import Hello5
-- import qualified Hello5

talk :: IO ()
talk = do
  putStrLn hello1
  putStrLn goodbye1
