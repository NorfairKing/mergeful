{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Mergeful.DirForest where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.DirForest
import Data.Mergeful.Item
import Data.Mergeful.Timed
import Data.Validity
import GHC.Generics (Generic)

data DirForestSyncRequest a = DirForestSyncRequest
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (DirForestSyncRequest a))

instance Validity a => Validity (DirForestSyncRequest a)

instance HasCodec a => HasCodec (DirForestSyncRequest a) where
  codec = object "DirForestSyncRequest" $ pure DirForestSyncRequest

data DirForestSyncResponse a = DirForestSyncResponse
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (DirForestSyncResponse a))

instance Validity a => Validity (DirForestSyncResponse a)

instance HasCodec a => HasCodec (DirForestSyncResponse a) where
  codec = object "DirForestSyncResponse" $ pure DirForestSyncResponse

newtype ClientDirForest a = ClientDirForest
  { clientDirForest :: DirForest (ClientItem a)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ClientDirForest a))

instance Validity a => Validity (ClientDirForest a)

instance HasCodec a => HasCodec (ClientDirForest a) where
  codec = dimapCodec ClientDirForest clientDirForest codec

newtype ServerDirForest a = ServerDirForest
  { serverDirForest :: DirForest (Timed a)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ServerDirForest a))

instance Validity a => Validity (ServerDirForest a)

instance HasCodec a => HasCodec (ServerDirForest a) where
  codec = dimapCodec ServerDirForest serverDirForest codec

makeDirForestSyncRequest :: ClientDirForest a -> DirForestSyncRequest a
makeDirForestSyncRequest = undefined

processServerSync :: ServerDirForest a -> DirForestSyncRequest a -> (ServerDirForest a, DirForestSyncResponse a)
processServerSync = undefined

mergeDirForestSyncResponse :: ClientDirForest a -> DirForestSyncResponse a -> ClientDirForest a
mergeDirForestSyncResponse = undefined
