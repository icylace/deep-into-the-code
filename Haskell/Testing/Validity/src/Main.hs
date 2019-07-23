{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

import Data.GenValidity
import Data.Validity

-- -----------------------------------------------------------------------------

data MyType = MyType
  { myRational :: Rational
  , myBool :: Bool
  } deriving (Show, Eq, Generic)

instance Validity MyType        -- Implementation is derived via `Generic`.
instance GenUnchecked MyType    -- Implementation is derived via `Generic`
instance GenValid MyType        -- Default implementation via `GenUnchecked` and `Validity`.

-- -----------------------------------------------------------------------------

-- TODO:

-- main :: IO ()
-- main = prettyValidate { myRational = 2.0, myBool = False }
