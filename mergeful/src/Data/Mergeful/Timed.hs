{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Mergeful.Timed
  ( ServerTime
  , initialServerTime
  , incrementServerTime
  , Timed(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson as JSON
import Data.Validity
import Data.Word

newtype ServerTime =
  ServerTime
    { unServerTime :: Word64
    }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance Validity ServerTime

initialServerTime :: ServerTime
initialServerTime = ServerTime 0

incrementServerTime :: ServerTime -> ServerTime
incrementServerTime (ServerTime w) = ServerTime (succ w)

data Timed a =
  Timed
    { timedValue :: !a
    , timedTime :: !ServerTime
    }
  deriving (Show, Eq, Generic)

instance Validity a => Validity (Timed a)

instance FromJSON a => FromJSON (Timed a) where
  parseJSON = withObject "Timed" $ \o -> Timed <$> o .: "value" <*> o .: "time"

instance ToJSON a => ToJSON (Timed a) where
  toJSON Timed {..} = object ["value" .= timedValue, "time" .= timedTime]
