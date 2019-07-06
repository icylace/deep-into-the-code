-- The module name and the filepath that contains it must match.
module Modules where

-- By default an import brings into scope all exports from a given module.
import Hello1

-- Importing a module that has no exports does nothing.
import Hello2

import Hello3

-- A qualified import requires the module name be prefixed to declarations when
-- they are used.
import qualified Hello4

-- A qualified import can be given an alias.
import qualified Hello5 as H5

-- A qualified import identifies where declarations come from. It disambiguates
-- between modules that export similarly named declarations.

-- We can selectively import specific declarations.
import Hello6 (goodbye6)

-- Though unnecessary, a module can be imported in the same way multiple times.
import Hello7
import Hello7

-- Multiple imports of the same module can differ in terms of explicitness.
import Hello8
import Hello8 (hello8)

-- Multiple imports of the same module can import different declarations.
import Hello9 (hello9)
import Hello9 (goodbye9)

-- Multiple explicit imports from the same module are listed comma-separated.
import Hello10 (hello10, goodbye10)

-- Multiple imports of the same module can differ in terms of qualification.
import Hello11
import qualified Hello11
import qualified Hello11 as H11

import MoreModules.Hello12
import MoreModules.EvenMoreModules.Hello13
import qualified MoreModules.EvenMoreModules.Hello14

-- Module names cannot contain periods.

-- Imported modules are top-level declarations. The entities imported as part of
-- those declarations, like other top-level declarations, have scope throughout
-- the module, although they can be shadowed by local bindings. The effect of
-- multiple import declarations is cumulative, but the ordering of import
-- declarations is irrelevant. An entity is in scope for the entire
-- module if it is imported by any of the import declarations.

talk :: IO ()
talk = do
  putStrLn hello1
  putStrLn goodbye1

  {-
  -- `Hello2` exports nothing, so its declarations cannot be imported.
  putStrLn hello2
  putStrLn goodbye2
  -}

  putStrLn hello3

  {-
  -- `Hello3` only explicitly exports `hello3`.
  putStrLn goodbye3
  -}

  putStrLn Hello4.hello4
  putStrLn Hello4.goodbye4

  putStrLn H5.hello5
  putStrLn H5.goodbye5

  {-
  -- `Hello6` exports `hello6` but only `goodbye6` is explicitly imported.
  putStrLn hello6
  -}

  putStrLn goodbye6

  putStrLn hello7
  putStrLn goodbye7

  putStrLn hello8
  putStrLn goodbye8
  putStrLn $ show $ FooString "hi"

  putStrLn hello9
  putStrLn goodbye9

  putStrLn hello10
  putStrLn goodbye10

  putStrLn hello11
  putStrLn goodbye11
  putStrLn Hello11.hello11
  putStrLn Hello11.goodbye11
  putStrLn H11.hello11
  putStrLn H11.goodbye11

  putStrLn hello12
  putStrLn goodbye12

  putStrLn hello13
  putStrLn goodbye13

  putStrLn MoreModules.EvenMoreModules.Hello14.hello14
  putStrLn MoreModules.EvenMoreModules.Hello14.goodbye14

{-
-- Imports must appear before other types of code.
import Hello1
-}
