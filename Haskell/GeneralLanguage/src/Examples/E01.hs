module Examples.E01 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4

textUUID :: IO Text
textUUID = fmap (T.pack . UUID.toString) UUIDv4.nextRandom

textUUID' :: IO Text
textUUID' = T.pack . UUID.toString <$> UUIDv4.nextRandom
