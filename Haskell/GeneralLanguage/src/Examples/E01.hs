{-# LANGUAGE ImportQualifiedPost #-}

module Examples.E01 where

import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4

textUUID :: IO Text
textUUID = fmap (T.pack . UUID.toString) UUIDv4.nextRandom

textUUID' :: IO Text
textUUID' = T.pack . UUID.toString <$> UUIDv4.nextRandom
