{-# LANGUAGE FlexibleInstances #-}

module LanguageExtensions.FlexibleInstances.After where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany p = (fst p) > 42
