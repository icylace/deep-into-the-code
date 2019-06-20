-- TODO: complete this

module Modules () where

import Foo1

-- functions must be prefixed with `Foo1.`
import qualified Foo1

-- functions must be prefixed with `Foo1.` or `F1.`
import qualified Foo1 as F1

import Foo1 (sayHello)

import Foo2

import Foo3
import Foo3 (sayHello)
