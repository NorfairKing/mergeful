{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A way to synchronise a single item with safe merge conflicts.
--
-- The setup is as follows:
--
-- * A central server is set up to synchronise with
-- * Each client synchronises with the central server, but never with eachother
--
--
-- A central server should operate as follows:
--
-- * The server accepts a 'ValueSyncRequest'.
-- * The server performs operations according to the functionality of 'processServerValueSync'.
-- * The server respons with a 'ValueSyncResponse'.
--
--
-- A client should operate as follows:
--
-- * The client produces a 'ValueSyncRequest' with 'makeValueSyncRequest'.
-- * The client sends that request to the central server and gets a 'ValueSyncResponse'.
-- * The client then updates its local store with 'mergeValueSyncResponseRaw' or 'mergeValueSyncResponseIgnoreProblems'.
--
--
-- WARNING:
-- This whole approach can break down if a server resets its server times
-- or if a client syncs with two different servers using the same server times.
module Data.Mergeful.Value
  ( ClientValue(..)
  , ValueSyncRequest(..)
  , makeValueSyncRequest
  , ValueSyncResponse(..)
  , MergeResult(..)
  , mergeValueSyncResponseRaw
  , mergeValueSyncResponseIgnoreProblems
  , ignoreMergeProblems
  , ServerTime
  , initialServerTime
  , incrementServerTime
  , ServerValue(..)
  , initialServerValue
  , processServerValueSync
  , Timed(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson as JSON
import Data.Validity

import Data.Mergeful.Timed

data ClientValue a
  -- | There is a value and it has been synced with the server.
  = ClientValueSynced !(Timed a)
  -- | There is a value and it has been synced with the server, but it has since been modified.
  | ClientValueSyncedButChanged !(Timed a)
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ClientValue a)

instance FromJSON a => FromJSON (ClientValue a) where
  parseJSON =
    withObject "ClientValue" $ \o -> do
      typ <- o .: "type"
      case typ :: String of
        "synced" -> ClientValueSynced <$> (Timed <$> o .: "value" <*> o .: "time")
        "changed" -> ClientValueSyncedButChanged <$> (Timed <$> o .: "value" <*> o .: "time")
        _ -> fail "unknown item type"

instance ToJSON a => ToJSON (ClientValue a) where
  toJSON ci =
    object $
    case ci of
      ClientValueSynced Timed {..} ->
        ["type" .= ("synced" :: String), "value" .= timedValue, "time" .= timedTime]
      ClientValueSyncedButChanged Timed {..} ->
        ["type" .= ("changed" :: String), "value" .= timedValue, "time" .= timedTime]

data ServerValue a =
  ServerValue !(Timed a)
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ServerValue a)

instance FromJSON a => FromJSON (ServerValue a) where
  parseJSON =
    withObject "ServerValue" $ \o -> ServerValue <$> (Timed <$> o .: "value" <*> o .: "time")

instance ToJSON a => ToJSON (ServerValue a) where
  toJSON (ServerValue Timed {..}) = object ["value" .= timedValue, "time" .= timedTime]

initialServerValue :: a -> ServerValue a
initialServerValue a = ServerValue $ Timed {timedValue = a, timedTime = initialServerTime}

data ValueSyncRequest a
  -- | There is an item locally that was synced at the given 'ServerTime'
  = ValueSyncRequestKnown !ServerTime
  -- | There is an item locally that was synced at the given 'ServerTime'
  -- but it has been changed since then.
  | ValueSyncRequestKnownButChanged !(Timed a)
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ValueSyncRequest a)

instance FromJSON a => FromJSON (ValueSyncRequest a) where
  parseJSON =
    withObject "ValueSyncRequest" $ \o -> do
      typ <- o .: "type"
      case typ :: String of
        "synced" -> ValueSyncRequestKnown <$> o .: "time"
        "changed" -> ValueSyncRequestKnownButChanged <$> (Timed <$> o .: "value" <*> o .: "time")
        _ -> fail "unknown item type"

instance ToJSON a => ToJSON (ValueSyncRequest a) where
  toJSON ci =
    object $
    let o n rest = ("type" .= (n :: String)) : rest
     in case ci of
          ValueSyncRequestKnown t -> o "synced" ["time" .= t]
          ValueSyncRequestKnownButChanged Timed {..} ->
            o "changed" ["value" .= timedValue, "time" .= timedTime]

data ValueSyncResponse a
  -- | The client and server are fully in sync.
  --
  -- Nothing needs to be done at the client side.
  = ValueSyncResponseInSyncFull
  -- | The client changed an item and server has succesfully been made aware of that.
  --
  -- The client needs to update its server time
  | ValueSyncResponseClientChanged !ServerTime
  -- | This item has been modified on the server side.
  --
  -- The client should modify it too.
  | ValueSyncResponseServerChanged !(Timed a)
  -- | A conflict occurred.
  --
  -- The server and the client both have an item, but it is different.
  -- The server kept its part, the client can either take whatever the server gave them
  -- or deal with the conflict somehow, and then try to re-sync.
  | ValueSyncResponseConflict !a -- ^ The item at the server side
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ValueSyncResponse a)

instance FromJSON a => FromJSON (ValueSyncResponse a) where
  parseJSON =
    withObject "ValueSyncResponse" $ \o -> do
      typ <- o .: "type"
      case typ :: String of
        "in-sync-full" -> pure ValueSyncResponseInSyncFull
        "client-changed" -> ValueSyncResponseClientChanged <$> o .: "time"
        "server-changed" ->
          ValueSyncResponseServerChanged <$> (Timed <$> o .: "value" <*> o .: "time")
        "conflict" -> ValueSyncResponseConflict <$> o .: "value"
        _ -> fail "unknown type"

instance ToJSON a => ToJSON (ValueSyncResponse a) where
  toJSON isr =
    object $
    let o s rest = ("type" .= (s :: String)) : rest
        oe s = o s []
     in case isr of
          ValueSyncResponseInSyncFull -> oe "in-sync-full"
          ValueSyncResponseClientChanged t -> o "client-changed" ["time" .= t]
          ValueSyncResponseServerChanged Timed {..} ->
            o "server-changed" ["value" .= timedValue, "time" .= timedTime]
          ValueSyncResponseConflict a -> o "conflict" ["value" .= a]

makeValueSyncRequest :: ClientValue a -> ValueSyncRequest a
makeValueSyncRequest cs =
  case cs of
    ClientValueSynced t -> ValueSyncRequestKnown (timedTime t)
    ClientValueSyncedButChanged t -> ValueSyncRequestKnownButChanged t

data MergeResult a
  -- | The merger went succesfully, no conflicts or desyncs
  = MergeSuccess !(ClientValue a)
  -- | There was a merge conflict. The server and client had different, conflicting versions.
  | MergeConflict
      !a -- ^ The item at the client side
      !a -- ^ The item at the server side
  -- | The server responded with a response that did not make sense given the client's request.
  | MergeMismatch
  deriving (Show, Eq, Generic)

instance Validity a => Validity (MergeResult a)

mergeValueSyncResponseRaw :: ClientValue a -> ValueSyncResponse a -> MergeResult a
mergeValueSyncResponseRaw cs sr =
  case cs of
    ClientValueSynced t ->
      case sr of
        ValueSyncResponseInSyncFull -> MergeSuccess $ ClientValueSynced t
        ValueSyncResponseServerChanged st -> MergeSuccess $ ClientValueSynced st
        _ -> MergeMismatch
    ClientValueSyncedButChanged ct ->
      case sr of
        ValueSyncResponseClientChanged st -> MergeSuccess $ ClientValueSynced $ ct {timedTime = st}
        ValueSyncResponseConflict si -> MergeConflict (timedValue ct) si
        _ -> MergeMismatch

mergeValueSyncResponseIgnoreProblems :: ClientValue a -> ValueSyncResponse a -> ClientValue a
mergeValueSyncResponseIgnoreProblems cs = ignoreMergeProblems cs . mergeValueSyncResponseRaw cs

ignoreMergeProblems :: ClientValue a -> MergeResult a -> ClientValue a
ignoreMergeProblems cs mr =
  case mr of
    MergeSuccess cs' -> cs'
    MergeConflict _ _ -> cs
    MergeMismatch -> cs

processServerValueSync ::
     ServerValue a -> ValueSyncRequest a -> (ValueSyncResponse a, ServerValue a)
processServerValueSync sv@(ServerValue (Timed si st)) sr =
  let t = incrementServerTime st
   in case sr of
        ValueSyncRequestKnown ct ->
          if ct >= st
                -- The client time is equal to the server time.
                -- The client indicates that the item was not modified at their side.
                -- This means that the items are in sync.
                -- (Unless the server somehow modified the item but not its server time,
                -- which would beconsidered a bug.)
            then (ValueSyncResponseInSyncFull, sv)
                -- The client time is less than the server time
                -- That means that the server has synced with another client in the meantime.
                -- Since the client indicates that the item was not modified at their side,
                -- we can just send it back to the client to have them update their version.
                -- No conflict here.
            else (ValueSyncResponseServerChanged (Timed {timedValue = si, timedTime = st}), sv)
        ValueSyncRequestKnownButChanged (Timed {timedValue = ci, timedTime = ct}) ->
          if ct >= st
                -- The client time is equal to the server time.
                -- The client indicates that the item *was* modified at their side.
                -- This means that the server needs to be updated.
            then ( ValueSyncResponseClientChanged t
                 , ServerValue (Timed {timedValue = ci, timedTime = t}))
                -- The client time is less than the server time
                -- That means that the server has synced with another client in the meantime.
                -- Since the client indicates that the item *was* modified at their side,
                -- there is a conflict.
            else (ValueSyncResponseConflict si, sv)
