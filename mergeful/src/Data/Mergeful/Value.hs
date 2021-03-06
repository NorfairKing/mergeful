{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A way to synchronise a single value with safe merge conflicts.
--
-- The setup is as follows:
--
-- * A central server is set up to synchronise with
-- * Each client synchronises with the central server, but never with eachother
--
--
--
-- = A client should operate as follows:
--
-- == For the first sychronisation
--
-- The client should ask the server for the current server value.
-- The server should send over a 'Timed' vaule, and the client should create its 'ClientValue' with 'initialClientValue'.
--
-- == For any following synchronisation:
--
--   * The client produces a 'ValueSyncRequest' with 'makeValueSyncRequest'.
--   * The client sends that request to the central server and gets a 'ValueSyncResponse'.
--   * The client then updates its local store with 'mergeValueSyncResponseRaw' or 'mergeValueSyncResponseIgnoreProblems'.
--
--
-- = The central server should operate as follows:
--
-- * The server should create an initial 'ServerValue' using 'initialServerValue'.
-- * The server accepts a 'ValueSyncRequest'.
-- * The server performs operations according to the functionality of 'processServerValueSync'.
-- * The server respons with a 'ValueSyncResponse'.
--
--
-- WARNING:
-- This whole approach can break down if a server resets its server times
-- or if a client syncs with two different servers using the same server times.
module Data.Mergeful.Value
  ( initialClientValue,
    makeValueSyncRequest,
    mergeValueSyncResponseRaw,
    ValueMergeResult (..),
    mergeValueSyncResponseIgnoreProblems,
    mergeIgnoringProblems,
    mergeFromServer,
    mergeUsingFunction,

    -- * Server side
    initialServerValue,
    processServerValueSync,

    -- * Types, for reference
    ChangedFlag (..),
    ClientValue (..),
    ValueSyncRequest (..),
    ValueSyncResponse (..),
    ServerValue (..),
  )
where

import Control.DeepSeq
import Data.Aeson as JSON
import Data.Mergeful.Timed
import Data.Validity
import GHC.Generics (Generic)

data ChangedFlag
  = Changed
  | NotChanged
  deriving (Show, Eq, Generic)

instance Validity ChangedFlag

instance NFData ChangedFlag

-- | The client side value.
--
-- The only differences between `a` and 'ClientValue a' are that
-- 'ClientValue a' also remembers the last synchronisation time from
-- the server, and whether the item has been modified at the client
--
-- There cannot be an unsynced 'ClientValue'.
data ClientValue a
  = ClientValue !(Timed a) !ChangedFlag
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ClientValue a)

instance NFData a => NFData (ClientValue a)

instance FromJSON a => FromJSON (ClientValue a) where
  parseJSON =
    withObject "ClientValue" $ \o ->
      ClientValue <$> (Timed <$> o .: "value" <*> o .: "time")
        <*> ( ( \b ->
                  if b
                    then Changed
                    else NotChanged
              )
                <$> o .: "changed"
            )

instance ToJSON a => ToJSON (ClientValue a) where
  toJSON (ClientValue Timed {..} cf) =
    object
      [ "value" .= timedValue,
        "time" .= timedTime,
        "changed"
          .= ( case cf of
                 Changed -> True
                 NotChanged -> False
             )
      ]

-- | Produce a client value based on an initial synchronisation request
initialClientValue :: Timed a -> ClientValue a
initialClientValue t = ClientValue t NotChanged

-- | The server-side value.
--
-- The only difference between 'a' and 'ServerValue a' is that 'ServerValue a' also
-- remembers the last time this value was changed during synchronisation.
newtype ServerValue a
  = ServerValue (Timed a)
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ServerValue a)

instance NFData a => NFData (ServerValue a)

instance FromJSON a => FromJSON (ServerValue a) where
  parseJSON =
    withObject "ServerValue" $ \o -> ServerValue <$> (Timed <$> o .: "value" <*> o .: "time")

instance ToJSON a => ToJSON (ServerValue a) where
  toJSON (ServerValue Timed {..}) = object ["value" .= timedValue, "time" .= timedTime]

-- | Initialise a server value.
--
-- Note that the server has to start with a value, the value 'a' cannot be omitted.
initialServerValue :: a -> ServerValue a
initialServerValue a = ServerValue $ Timed {timedValue = a, timedTime = initialServerTime}

data ValueSyncRequest a
  = -- | There is an item locally that was synced at the given 'ServerTime'
    ValueSyncRequestKnown !ServerTime
  | -- | There is an item locally that was synced at the given 'ServerTime'
    -- but it has been changed since then.
    ValueSyncRequestKnownButChanged !(Timed a)
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ValueSyncRequest a)

instance NFData a => NFData (ValueSyncRequest a)

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
  = -- | The client and server are fully in sync.
    --
    -- Nothing needs to be done at the client side.
    ValueSyncResponseInSync
  | -- | The client changed the value and server has succesfully been made aware of that.
    --
    -- The client needs to update its server time
    ValueSyncResponseClientChanged !ServerTime
  | -- | This value has been changed on the server side.
    --
    -- The client should change it too.
    ValueSyncResponseServerChanged !(Timed a)
  | -- | The item at the server side
    ValueSyncResponseConflict !(Timed a)
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ValueSyncResponse a)

instance NFData a => NFData (ValueSyncResponse a)

instance FromJSON a => FromJSON (ValueSyncResponse a) where
  parseJSON =
    withObject "ValueSyncResponse" $ \o -> do
      typ <- o .: "type"
      case typ :: String of
        "in-sync" -> pure ValueSyncResponseInSync
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
            ValueSyncResponseInSync -> oe "in-sync"
            ValueSyncResponseClientChanged t -> o "client-changed" ["time" .= t]
            ValueSyncResponseServerChanged Timed {..} ->
              o "server-changed" ["value" .= timedValue, "time" .= timedTime]
            ValueSyncResponseConflict a -> o "conflict" ["value" .= a]

-- | Produce an 'ItemSyncRequest' from a 'ClientItem'.
--
-- Send this to the server for synchronisation.
makeValueSyncRequest :: ClientValue a -> ValueSyncRequest a
makeValueSyncRequest (ClientValue t cf) =
  case cf of
    NotChanged -> ValueSyncRequestKnown (timedTime t)
    Changed -> ValueSyncRequestKnownButChanged t

data ValueMergeResult a
  = -- | The merger went succesfully, no conflicts or desyncs
    MergeSuccess !(ClientValue a)
  | -- | The item at the server side
    MergeConflict !a !(Timed a)
  | -- | The server responded with a response that did not make sense given the client's request.
    --
    -- This should not happen in practice.
    MergeMismatch
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ValueMergeResult a)

instance NFData a => NFData (ValueMergeResult a)

-- | Merge an 'ValueSyncResponse' into the current 'ClientValue'.
--
-- This function will not make any decisions about what to do with
-- conflicts or mismatches between the request and the response.
-- It only produces a 'ValueMergeResult' so you can decide what to do with it.
mergeValueSyncResponseRaw :: ClientValue a -> ValueSyncResponse a -> ValueMergeResult a
mergeValueSyncResponseRaw cv@(ClientValue ct cf) sr =
  case cf of
    NotChanged ->
      case sr of
        ValueSyncResponseInSync -> MergeSuccess cv
        ValueSyncResponseServerChanged st -> MergeSuccess $ ClientValue st NotChanged
        _ -> MergeMismatch
    Changed ->
      case sr of
        ValueSyncResponseClientChanged st ->
          MergeSuccess $ ClientValue (ct {timedTime = st}) NotChanged
        ValueSyncResponseConflict si -> MergeConflict (timedValue ct) si
        _ -> MergeMismatch

-- | Resolve a 'ValueSyncResponse' into the current 'ClientValue'.
--
-- This function ignores any problems that may occur.
-- In the case of a conclict, it will just not update the client item.
-- The next sync request will then produce a conflict again.
--
-- > mergeValueSyncResponseIgnoreProblems cs = mergeIgnoringProblems cs . mergeValueSyncResponseRaw cs
mergeValueSyncResponseIgnoreProblems :: ClientValue a -> ValueSyncResponse a -> ClientValue a
mergeValueSyncResponseIgnoreProblems cs = mergeIgnoringProblems cs . mergeValueSyncResponseRaw cs

-- | Ignore any merge problems in a 'ValueMergeResult'.
--
-- This function just returns the original 'ClientValue' if anything other than 'MergeSuccess' occurs.
--
-- This function ignores any problems that may occur.
-- In the case of a conclict, it will just not update the client item.
-- The next sync request will then produce a conflict again.
--
-- Pro: does not lose data
--
-- __Con: Clients will diverge when a conflict occurs__
mergeIgnoringProblems :: ClientValue a -> ValueMergeResult a -> ClientValue a
mergeIgnoringProblems cs mr =
  case mr of
    MergeSuccess cs' -> cs'
    MergeConflict _ _ -> cs
    MergeMismatch -> cs

-- | Resolve a 'ValueMergeResult' using a given merge strategy.
--
-- This function ignores 'MergeMismatch' and will just return the original 'ClientValue' in that case.
--
-- In order for clients to converge on the same value correctly, this function must be:
--
-- * Associative
-- * Idempotent
-- * The same on all clients
mergeUsingFunction ::
  (a -> Timed a -> Timed a) -> ClientValue a -> ValueMergeResult a -> ClientValue a
mergeUsingFunction func cs mr =
  case mr of
    MergeSuccess cs' -> cs'
    MergeConflict a1 a2 -> ClientValue (func a1 a2) NotChanged
    MergeMismatch -> cs

-- | Resolve a 'ValueMergeResult' by taking whatever the server gave the client.
--
-- Pro: Clients will converge on the same value.
--
-- __Con: Conflicting updates will be lost.__
mergeFromServer :: ClientValue a -> ValueMergeResult a -> ClientValue a
mergeFromServer = mergeUsingFunction (\_ serverItem -> serverItem)

-- | Serve an 'ValueSyncRequest' using the current 'ServerValue', producing an 'ValueSyncResponse' and a new 'ServerValue'.
processServerValueSync ::
  ServerValue a -> ValueSyncRequest a -> (ValueSyncResponse a, ServerValue a)
processServerValueSync sv@(ServerValue t@(Timed _ st)) sr =
  case sr of
    ValueSyncRequestKnown ct ->
      if ct >= st
        then-- The client time is equal to the server time.
        -- The client indicates that the item was not modified at their side.
        -- This means that the items are in sync.
        -- (Unless the server somehow modified the item but not its server time,
        -- which would beconsidered a bug.)
          (ValueSyncResponseInSync, sv)
        else-- The client time is less than the server time
        -- That means that the server has synced with another client in the meantime.
        -- Since the client indicates that the item was not modified at their side,
        -- we can just send it back to the client to have them update their version.
        -- No conflict here.
          (ValueSyncResponseServerChanged t, sv)
    ValueSyncRequestKnownButChanged Timed {timedValue = ci, timedTime = ct} ->
      if ct >= st
        then-- The client time is equal to the server time.
        -- The client indicates that the item *was* modified at their side.
        -- This means that the server needs to be updated.

          let st' = incrementServerTime st
           in ( ValueSyncResponseClientChanged st',
                ServerValue (Timed {timedValue = ci, timedTime = st'})
              )
        else-- The client time is less than the server time
        -- That means that the server has synced with another client in the meantime.
        -- Since the client indicates that the item *was* modified at their side,
        -- there is a conflict.
          (ValueSyncResponseConflict t, sv)
