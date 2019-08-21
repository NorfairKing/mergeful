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
-- * The server accepts a 'ItemSyncRequest'.
-- * The server performs operations according to the functionality of 'processServerItemSync'.
-- * The server respons with a 'ItemSyncResponse'.
--
--
-- A client should operate as follows:
--
-- * The client produces a 'ItemSyncRequest' with 'makeItemSyncRequest'.
-- * The client sends that request to the central server and gets a 'ItemSyncResponse'.
-- * The client then updates its local store with 'mergeItemSyncResponseRaw' or 'mergeItemSyncResponseIgnoreProblems'.
--
--
-- WARNING:
-- This whole approach can break down if a server resets its server times
-- or if a client syncs with two different servers using the same server times.
module Data.Mergeful.Item
  ( ClientItem(..)
  , ItemSyncRequest(..)
  , makeItemSyncRequest
  , ItemSyncResponse(..)
  , MergeResult(..)
  , mergeItemSyncResponseRaw
  , mergeItemSyncResponseIgnoreProblems
  , ignoreMergeProblems
  , ServerTime
  , initialServerTime
  , incrementServerTime
  , ServerItem(..)
  , initialServerItem
  , processServerItemSync
  , Timed(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson as JSON
import Data.Validity
import Data.Word

import Control.Applicative

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

data ClientItem a
  -- | There is no item on the client side
  = ClientEmpty
  -- | There is is an item but the server is not aware of it yet.
  | ClientAdded !a
  -- | There is is an item and it has been synced with the server.
  | ClientItemSynced !(Timed a)
  -- | There is is an item and it has been synced with the server, but it has since been modified.
  | ClientItemSyncedButChanged !(Timed a)
  -- | There was an item, and it has been deleted locally, but the server has not been made aware of this.
  | ClientDeleted !ServerTime
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ClientItem a)

instance FromJSON a => FromJSON (ClientItem a) where
  parseJSON =
    withObject "ClientItem" $ \o -> do
      typ <- o .: "type"
      case typ :: String of
        "empty" -> pure ClientEmpty
        "added" -> ClientAdded <$> o .: "value"
        "synced" -> ClientItemSynced <$> (Timed <$> o .: "value" <*> o .: "time")
        "changed" -> ClientItemSyncedButChanged <$> (Timed <$> o .: "value" <*> o .: "time")
        "deleted" -> ClientDeleted <$> o .: "time"
        _ -> fail "unknown item type"

instance ToJSON a => ToJSON (ClientItem a) where
  toJSON ci =
    object $
    case ci of
      ClientEmpty -> ["type" .= ("empty" :: String)]
      ClientAdded a -> ["type" .= ("added" :: String), "value" .= a]
      ClientItemSynced Timed {..} ->
        ["type" .= ("synced" :: String), "value" .= timedValue, "time" .= timedTime]
      ClientItemSyncedButChanged Timed {..} ->
        ["type" .= ("changed" :: String), "value" .= timedValue, "time" .= timedTime]
      ClientDeleted t -> ["type" .= ("deleted" :: String), "time" .= t]

data ServerItem a
  = ServerEmpty
  | ServerFull !(Timed a)
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ServerItem a)

instance FromJSON a => FromJSON (ServerItem a) where
  parseJSON =
    withObject "ServerItem" $ \o ->
      ServerFull <$> (Timed <$> o .: "value" <*> o .: "time") <|> pure ServerEmpty

instance ToJSON a => ToJSON (ServerItem a) where
  toJSON si =
    object $
    case si of
      ServerEmpty -> []
      ServerFull Timed {..} -> ["value" .= timedValue, "time" .= timedTime]

initialServerItem :: ServerItem a
initialServerItem = ServerEmpty

data ItemSyncRequest a
  -- | There is no item locally
  = ItemSyncRequestPoll
  -- | There is an item locally that hasn't been synced to the server yet.
  | ItemSyncRequestNew !a
  -- | There is an item locally that was synced at the given 'ServerTime'
  | ItemSyncRequestKnown !ServerTime
  -- | There is an item locally that was synced at the given 'ServerTime'
  -- but it has been changed since then.
  | ItemSyncRequestKnownButChanged !(Timed a)
  -- | There was an item locally that has been deleted but the
  -- deletion wasn't synced to the server yet.
  | ItemSyncRequestDeletedLocally !ServerTime
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ItemSyncRequest a)

instance FromJSON a => FromJSON (ItemSyncRequest a) where
  parseJSON =
    withObject "ItemSyncRequest" $ \o -> do
      typ <- o .: "type"
      case typ :: String of
        "empty" -> pure ItemSyncRequestPoll
        "added" -> ItemSyncRequestNew <$> o .: "value"
        "synced" -> ItemSyncRequestKnown <$> o .: "time"
        "changed" -> ItemSyncRequestKnownButChanged <$> (Timed <$> o .: "value" <*> o .: "time")
        "deleted" -> ItemSyncRequestDeletedLocally <$> o .: "time"
        _ -> fail "unknown item type"

instance ToJSON a => ToJSON (ItemSyncRequest a) where
  toJSON ci =
    object $
    let o n rest = ("type" .= (n :: String)) : rest
        oe n = o n []
     in case ci of
          ItemSyncRequestPoll -> oe "empty"
          ItemSyncRequestNew a -> o "added" ["value" .= a]
          ItemSyncRequestKnown t -> o "synced" ["time" .= t]
          ItemSyncRequestKnownButChanged Timed {..} ->
            o "changed" ["value" .= timedValue, "time" .= timedTime]
          ItemSyncRequestDeletedLocally t -> o "deleted" ["time" .= t]

data ItemSyncResponse a
  -- | The client and server are fully in sync, and both empty
  --
  -- Nothing needs to be done at the client side.
  = ItemSyncResponseInSyncEmpty
  -- | The client and server are fully in sync.
  --
  -- Nothing needs to be done at the client side.
  | ItemSyncResponseInSyncFull
  -- | The client added an item and server has succesfully been made aware of that.
  --
  -- The client needs to update its server time
  | ItemSyncResponseSuccesfullyAdded !ServerTime
  -- | The client changed an item and server has succesfully been made aware of that.
  --
  -- The client needs to update its server time
  | ItemSyncResponseSuccesfullyChanged !ServerTime
  -- | The client deleted an item and server has succesfully been made aware of that.
  --
  -- Nothing needs to be done at the client side.
  | ItemSyncResponseSuccesfullyDeleted
  -- | This item has been added on the server side
  --
  -- The client should add it too.
  | ItemSyncResponseNewAtServer !(Timed a)
  -- | This item has been modified on the server side.
  --
  -- The client should modify it too.
  | ItemSyncResponseModifiedAtServer !(Timed a)
  -- | The item was deleted on the server side
  --
  -- The client should delete it too.
  | ItemSyncResponseDeletedAtServer
  -- | A conflict occurred.
  --
  -- The server and the client both have an item, but it is different.
  -- The server kept its part, the client can either take whatever the server gave them
  -- or deal with the conflict somehow, and then try to re-sync.
  | ItemSyncResponseConflict !a -- ^ The item at the server side
  -- | A conflict occurred.
  --
  -- The server has an item but the client does not.
  -- The server kept its part, the client can either take whatever the server gave them
  -- or deal with the conflict somehow, and then try to re-sync.
  | ItemSyncResponseConflictClientDeleted !a -- ^ The item at the server side
  -- | A conflict occurred.
  --
  -- The client has a (modified) item but the server does not have any item.
  -- The server left its item deleted, the client can either delete its item too
  -- or deal with the conflict somehow, and then try to re-sync.
  | ItemSyncResponseConflictServerDeleted
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ItemSyncResponse a)

instance FromJSON a => FromJSON (ItemSyncResponse a) where
  parseJSON =
    withObject "ItemSyncResponse" $ \o -> do
      typ <- o .: "type"
      case typ :: String of
        "in-sync-empty" -> pure ItemSyncResponseInSyncEmpty
        "in-sync-full" -> pure ItemSyncResponseInSyncFull
        "client-added" -> ItemSyncResponseSuccesfullyAdded <$> o .: "time"
        "client-changed" -> ItemSyncResponseSuccesfullyChanged <$> o .: "time"
        "client-deleted" -> pure ItemSyncResponseSuccesfullyDeleted
        "server-added" -> ItemSyncResponseNewAtServer <$> (Timed <$> o .: "value" <*> o .: "time")
        "server-changed" ->
          ItemSyncResponseModifiedAtServer <$> (Timed <$> o .: "value" <*> o .: "time")
        "server-deleted" -> pure ItemSyncResponseDeletedAtServer
        "conflict" -> ItemSyncResponseConflict <$> o .: "value"
        "conflict-client-deleted" -> ItemSyncResponseConflictClientDeleted <$> o .: "value"
        "conflict-server-deleted" -> pure ItemSyncResponseConflictServerDeleted
        _ -> fail "unknown type"

instance ToJSON a => ToJSON (ItemSyncResponse a) where
  toJSON isr =
    object $
    let o s rest = ("type" .= (s :: String)) : rest
        oe s = o s []
     in case isr of
          ItemSyncResponseInSyncEmpty -> oe "in-sync-empty"
          ItemSyncResponseInSyncFull -> oe "in-sync-full"
          ItemSyncResponseSuccesfullyAdded t -> o "client-added" ["time" .= t]
          ItemSyncResponseSuccesfullyChanged t -> o "client-changed" ["time" .= t]
          ItemSyncResponseSuccesfullyDeleted -> oe "client-deleted"
          ItemSyncResponseNewAtServer Timed {..} ->
            o "server-added" ["value" .= timedValue, "time" .= timedTime]
          ItemSyncResponseModifiedAtServer Timed {..} ->
            o "server-changed" ["value" .= timedValue, "time" .= timedTime]
          ItemSyncResponseDeletedAtServer -> oe "server-deleted"
          ItemSyncResponseConflict a -> o "conflict" ["value" .= a]
          ItemSyncResponseConflictClientDeleted a -> o "conflict-client-deleted" ["value" .= a]
          ItemSyncResponseConflictServerDeleted -> oe "conflict-server-deleted"

makeItemSyncRequest :: ClientItem a -> ItemSyncRequest a
makeItemSyncRequest cs =
  case cs of
    ClientEmpty -> ItemSyncRequestPoll
    ClientAdded i -> ItemSyncRequestNew i
    ClientItemSynced t -> ItemSyncRequestKnown (timedTime t)
    ClientItemSyncedButChanged t -> ItemSyncRequestKnownButChanged t
    ClientDeleted st -> ItemSyncRequestDeletedLocally st

data MergeResult a
  -- | The merger went succesfully, no conflicts or desyncs
  = MergeSuccess !(ClientItem a)
  -- | There was a merge conflict. The server and client had different, conflicting versions.
  | MergeConflict
      !a -- ^ The item at the client side
      !a -- ^ The item at the server side
  -- | There was a merge conflict. The client had deleted the item while the server had modified it.
  | MergeConflictClientDeleted !a -- ^ The item at the server side
  -- | There was a merge conflict. The server had deleted the item while the client had modified it.
  | MergeConflictServerDeleted !a -- ^ The item at the client side
  -- | A desync was detected.
  --
  -- This only occurs if the server's time has been reset
  -- or if a client syncs with multiple servers with the same server time.
  | MergeDesync
      !ServerTime -- ^ Server time
      !(Maybe a) -- ^ Item at the server side
  -- | The server responded with a response that did not make sense given the client's request.
  | MergeMismatch
  deriving (Show, Eq, Generic)

instance Validity a => Validity (MergeResult a)

mergeItemSyncResponseRaw :: ClientItem a -> ItemSyncResponse a -> MergeResult a
mergeItemSyncResponseRaw cs sr =
  case cs of
    ClientEmpty ->
      case sr of
        ItemSyncResponseInSyncEmpty -> MergeSuccess cs
        ItemSyncResponseNewAtServer t -> MergeSuccess $ ClientItemSynced t
        _ -> MergeMismatch
    ClientAdded ci ->
      case sr of
        ItemSyncResponseSuccesfullyAdded st ->
          MergeSuccess $ ClientItemSynced $ Timed {timedValue = ci, timedTime = st}
        ItemSyncResponseConflict si -> MergeConflict ci si
        _ -> MergeMismatch
    ClientItemSynced t ->
      case sr of
        ItemSyncResponseInSyncFull -> MergeSuccess $ ClientItemSynced t
        ItemSyncResponseModifiedAtServer st -> MergeSuccess $ ClientItemSynced st
        ItemSyncResponseDeletedAtServer -> MergeSuccess ClientEmpty
        _ -> MergeMismatch
    ClientItemSyncedButChanged ct ->
      case sr of
        ItemSyncResponseSuccesfullyChanged st ->
          MergeSuccess $ ClientItemSynced $ ct {timedTime = st}
        ItemSyncResponseConflict si -> MergeConflict (timedValue ct) si
        ItemSyncResponseConflictServerDeleted -> MergeConflictServerDeleted (timedValue ct)
        _ -> MergeMismatch
    ClientDeleted _ ->
      case sr of
        ItemSyncResponseSuccesfullyDeleted -> MergeSuccess ClientEmpty
        ItemSyncResponseConflictClientDeleted si -> MergeConflictClientDeleted si
        _ -> MergeMismatch

mergeItemSyncResponseIgnoreProblems :: ClientItem a -> ItemSyncResponse a -> ClientItem a
mergeItemSyncResponseIgnoreProblems cs = ignoreMergeProblems cs . mergeItemSyncResponseRaw cs

ignoreMergeProblems :: ClientItem a -> MergeResult a -> ClientItem a
ignoreMergeProblems cs mr =
  case mr of
    MergeSuccess cs' -> cs'
    MergeConflict _ _ -> cs
    MergeConflictServerDeleted _ -> cs
    MergeConflictClientDeleted _ -> cs
    MergeDesync _ _ -> cs
    MergeMismatch -> cs

processServerItemSync :: ServerItem a -> ItemSyncRequest a -> (ItemSyncResponse a, ServerItem a)
processServerItemSync store sr =
  case store of
    ServerEmpty ->
      let t = initialServerTime
       in case sr of
            ItemSyncRequestPoll -> (ItemSyncResponseInSyncEmpty, store)
            ItemSyncRequestNew ci ->
              ( ItemSyncResponseSuccesfullyAdded t
              , ServerFull $ Timed {timedValue = ci, timedTime = t})
            ItemSyncRequestKnown _
             -- This indicates that the server synced with another client and was told to
             -- delete its item.
             --
             -- Given that the client indicates that it did not change anything locally,
             -- the server will just instruct the client to delete its item too.
             -- No conflict here.
             -> (ItemSyncResponseDeletedAtServer, store)
            ItemSyncRequestKnownButChanged _
             -- This indicates that the server synced with another client and was told to
             -- delete its item.
             --
             -- Given that the client indicates that it *did* change its item locally,
             -- there is a conflict.
             -> (ItemSyncResponseConflictServerDeleted, store)
            ItemSyncRequestDeletedLocally _
             -- This means that the server synced with another client,
             -- was instructed to delete its item by that client,
             -- and is now being told to delete its item again.
             --
             -- That's fine, it will just remain deleted.
             -- No conflict here
             -> (ItemSyncResponseSuccesfullyDeleted, store)
    ServerFull (Timed si st) ->
      let t = incrementServerTime st
       in case sr of
            ItemSyncRequestPoll
              -- The client is empty but the server is not.
              -- This means that the server has synced with another client before,
              -- so we can just send the item to the client.
             -> (ItemSyncResponseNewAtServer (Timed {timedValue = si, timedTime = st}), store)
            ItemSyncRequestNew _
              -- The client has a newly added item, so it thought it was empty before that,
              -- but the server has already synced with another client before.
              -- This indicates a conflict.
              -- The server is always right, so the item at the server will remain unmodified.
              -- The client will receive the conflict.
             -> (ItemSyncResponseConflict si, store)
            ItemSyncRequestKnown ct ->
              if ct >= st
                -- The client time is equal to the server time.
                -- The client indicates that the item was not modified at their side.
                -- This means that the items are in sync.
                -- (Unless the server somehow modified the item but not its server time,
                -- which would beconsidered a bug.)
                then (ItemSyncResponseInSyncFull, store)
                -- The client time is less than the server time
                -- That means that the server has synced with another client in the meantime.
                -- Since the client indicates that the item was not modified at their side,
                -- we can just send it back to the client to have them update their version.
                -- No conflict here.
                else ( ItemSyncResponseModifiedAtServer (Timed {timedValue = si, timedTime = st})
                     , store)
            ItemSyncRequestKnownButChanged (Timed {timedValue = ci, timedTime = ct}) ->
              if ct >= st
                -- The client time is equal to the server time.
                -- The client indicates that the item *was* modified at their side.
                -- This means that the server needs to be updated.
                then ( ItemSyncResponseSuccesfullyChanged t
                     , ServerFull (Timed {timedValue = ci, timedTime = t}))
                -- The client time is less than the server time
                -- That means that the server has synced with another client in the meantime.
                -- Since the client indicates that the item *was* modified at their side,
                -- there is a conflict.
                else (ItemSyncResponseConflict si, store)
            ItemSyncRequestDeletedLocally ct ->
              if ct >= st
                -- The client time is equal to the server time.
                -- The client indicates that the item was deleted on their side.
                -- This means that the server item needs to be deleted as well.
                then (ItemSyncResponseSuccesfullyDeleted, ServerEmpty)
                -- The client time is less than the server time
                -- That means that the server has synced with another client in the meantime.
                -- Since the client indicates that the item was deleted at their side,
                -- there is a conflict.
                else (ItemSyncResponseConflictClientDeleted si, store)
