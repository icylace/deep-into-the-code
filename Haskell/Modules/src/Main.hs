-- TODO:
-- show modules importing each other
-- show exported functions being used
-- show explicit data constructor importting
-- show recommended style for long lists of exports
-- talk about recommended compiler flags for enforcing export warnings

-- TODO: verify this:
-- -- You can use the same alias across imports.
-- import qualified Language.Haskell.HsColour           as HSC
-- import qualified Language.Haskell.HsColour.Colourise as HSC

-- TODO: consider this:
-- https://wiki.haskell.org/Import_modules_properly
-- import qualified Very.Special.Module as VSM
-- import Another.Important.Module (printf, (<|>), )
-- import Very.Special.Module
-- import Another.Important.Module hiding (open, close, )

-- -----------------------------------------------------------------------------

-- By default an import brings into scope all exports from a module.

import Hello1

-- Importing a module that has no exports does nothing.

import Hello2

-- We can only import what is exported by a module.

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

import Hello12 (hello12)
import qualified Hello12 (goodbye12)

import qualified Hello13 as H13 (hello13)

-- TODO: try out these:
-- import HelloX as HX
-- import HelloX as HX (helloX)
-- import HelloX (helloX) as HX
-- import qualified HelloX (helloX) as HX

-- Importing a datatype's data constructors can be done implicitly...

import Hello14 (Hello14(..))

-- ...or explicitly.

import Hello15 (Hello15(Hi15))

import Hello16

import Hello17

import Hello18

import Hello19

import MoreModules.Hello20

import MoreModules.EvenMoreModules.Hello21

import qualified MoreModules.EvenMoreModules.Hello22

-- Module names cannot contain periods.

-- Imported modules are top-level declarations. The entities imported as part of
-- those declarations, like other top-level declarations, have scope throughout
-- the module, although they can be shadowed by local bindings. The effect of
-- multiple import declarations is cumulative, but the ordering of import
-- declarations is irrelevant. An entity is in scope for the entire
-- module if it is imported by any of the import declarations.

main :: IO ()
main = do
  putStrLn hello1
  putStrLn goodbye1

  {-
  -- `Hello2` exports nothing, so its declarations cannot be imported.
  putStrLn hello2      -- Error.
  putStrLn goodbye2    -- Error.
  -}

  putStrLn hello3

  {-
  -- `Hello3` only explicitly exports `hello3`.
  putStrLn goodbye3    -- Error.
  -}

  putStrLn Hello4.hello4
  putStrLn Hello4.goodbye4

  {-
  -- Since `Hello5` was given an alias its alias must used instead.
  putStrLn Hello5.hello5      -- Error.
  putStrLn Hello5.goodbye5    -- Error.
  -}

  putStrLn H5.hello5
  putStrLn H5.goodbye5

  {-
  -- `Hello6` exports `hello6` but only `goodbye6` is explicitly imported.
  putStrLn hello6    -- Error.
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
  putStrLn Hello12.goodbye12

  {-
  putStrLn hello13    -- Error.
  -}

  putStrLn H13.hello13

  putStrLn $ show Hi14
  putStrLn $ show Bye14

  putStrLn $ show Hi15

  {-
  -- The import of the `Hello15` datatype only had the `Hi15` data constructor.
  putStrLn $ show Bye15    -- Error.
  -}

  {-
  -- `Hello16` exports the `Hello16` datatype without its data constructors.
  putStrLn $ show Hi16     -- Error.
  putStrLn $ show Bye16    -- Error.
  -}

  {-
  -- `Hello17` exports `Hello17` datatype with empty list of data constructors.
  putStrLn $ show Hi17     -- Error.
  putStrLn $ show Bye17    -- Error.
  -}

  {-
  -- `Hello18` exports `Hello18` datatype with only `Bye18` data constructor.
  putStrLn $ show $ Hi18    -- Error.
  -}

  putStrLn $ show Bye18

  putStrLn $ show Hi19
  putStrLn $ show Bye19

  putStrLn hello20
  putStrLn goodbye20

  putStrLn hello21
  putStrLn goodbye21

  putStrLn MoreModules.EvenMoreModules.Hello22.hello22
  putStrLn MoreModules.EvenMoreModules.Hello22.goodbye22

{-
-- Imports must appear before any expressions.

import Hello1    -- Error.
-}
