{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Dealing with server times.
--
-- __If you are importing this module, you are probably doing something wrong.__
module Data.Mergeful.Timed
  ( ServerTime (..),
    initialServerTime,
    incrementServerTime,
    Timed (..),
  )
where

import Control.DeepSeq
import Data.Aeson as JSON
import Data.Validity
import Data.Word
import GHC.Generics (Generic)

-- | A "time", as "measured" by the server.
--
-- This is closer to a version number than an actual timestamp, but that
-- distinction should not matter for your usage of this library.
--
-- In any case, a client should not be changing this value.
--
-- We use a 'Word64' instead of a natural.
-- This will go wrong after 2^64 versions, but since that
-- will not happen in practice, we will not worry about it.
-- You would have to sync millions of modifications every second
-- until long after the sun consumes the earth for this to be a problem.
newtype ServerTime
  = ServerTime
      { unServerTime :: Word64
      }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance Validity ServerTime

instance NFData ServerTime

-- | A server time to start with.
initialServerTime :: ServerTime
initialServerTime = ServerTime 0

-- | Increment a server time.
incrementServerTime :: ServerTime -> ServerTime
incrementServerTime (ServerTime w) = ServerTime (succ w)

-- | A value along with a server time.
data Timed a
  = Timed
      { timedValue :: !a,
        timedTime :: !ServerTime
      }
  deriving (Show, Eq, Generic)

instance Validity a => Validity (Timed a)

instance NFData a => NFData (Timed a)

instance FromJSON a => FromJSON (Timed a) where
  parseJSON = withObject "Timed" $ \o -> Timed <$> o .: "value" <*> o .: "time"

instance ToJSON a => ToJSON (Timed a) where
  toJSON Timed {..} = object ["value" .= timedValue, "time" .= timedTime]
