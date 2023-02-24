{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
-- Remove when the dirforest instance is in the right place.
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Mergeful.DirForest where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.DirForest
import Data.Mergeful.Timed
import Data.Validity
import GHC.Generics (Generic)

data DirForestSyncRequest a = DirForestSyncRequest
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (DirForestSyncRequest a)

instance Validity a => Validity (DirForestSyncRequest a)

instance HasCodec a => HasCodec (DirForestSyncRequest a) where
  codec = undefined

data DirForestSyncResponse a = DirForestSyncResponse
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (DirForestSyncResponse a)

instance Validity a => Validity (DirForestSyncResponse a)

instance HasCodec a => HasCodec (DirForestSyncResponse a) where
  codec = undefined

data ClientDirForest a = ClientDirForest
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (ClientDirForest a)

instance Validity a => Validity (ClientDirForest a)

instance HasCodec a => HasCodec (ClientDirForest a) where
  codec = undefined

newtype ServerDirForest a = ServerDirForest
  {serverDirForest :: DirForest (Timed a)}
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (ServerDirForest a)

instance Validity a => Validity (ServerDirForest a)

instance HasCodec a => HasCodec (ServerDirForest a) where
  codec = dimapCodec ServerDirForest serverDirForest codec

-- Move to the dirforest library
instance HasCodec a => HasCodec (DirForest a) where
  codec = undefined

makeDirForestSyncRequest :: ClientDirForest a -> DirForestSyncRequest a
makeDirForestSyncRequest = undefined

processServerSync :: ServerDirForest a -> DirForestSyncRequest a -> (ServerDirForest a, DirForestSyncResponse a)
processServerSync = undefined

mergeDirForestSyncResponse :: ClientDirForest a -> DirForestSyncResponse a -> ClientDirForest a
mergeDirForestSyncResponse = undefined
