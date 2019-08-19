{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A way to synchronise items without merge conflicts.
--
-- This concept has a few requirements:
--
-- * Items must be immutable.
-- * Items must allow for a centrally unique identifier.
-- * Identifiers for items must be generatable in such a way that they are certainly unique.
--
-- Should mutation be a requirement, then it can be build such that it entails deleting the old version and creating a new version that is the modification of the old version.
--
--
-- There are a few obvious candidates for identifiers:
--
-- * incremental identifiers
-- * universally unique identifiers (recommended).
--
--
--
-- The typical setup is as follows:
--
-- * A central server is set up to synchronise with
-- * Each client synchronises with the central server, but never with eachother
--
--
-- A central server should operate as follows:
--
-- * The server accepts a 'SyncRequest'.
-- * The server performs operations according to the functionality of 'processSync'.
-- * The server respons with a 'SyncResponse'.
--
--
-- A client should operate as follows:
--
-- * The client produces a 'SyncRequest' with 'makeSyncRequest'.
-- * The client sends that request to the central server and gets a 'SyncResponse'.
-- * The client then updates its local store with 'mergeSyncResponse'.
module Data.Mergeful
  ( ClientStore(..)
  , SyncRequest(..)
  , makeSyncRequest
  , SyncResponse(..)
  , MergeResult(..)
  , mergeSyncResponseRaw
  , mergeSyncResponseIgnoreProblems
  , ignoreMergeProblems
  , ServerTime(..)
  , initialServerTime
  , incrementServerTime
  , ServerStore(..)
  , initialServerStore
  , processServerSync
  ) where

import GHC.Generics (Generic)

import Data.Validity
import Data.Word

import Control.Exception (assert)

data ClientStore a
  = ClientEmpty
  | ClientAdded a
  | ClientSynced a ServerTime
  | ClientSyncedButChanged a ServerTime -- The item has been synced with the server but since modified.
  | ClientDeleted ServerTime
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ClientStore a)

newtype ServerTime =
  ServerTime
    { unServerTime :: Word64
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity ServerTime

initialServerTime :: ServerTime
initialServerTime = ServerTime 0

incrementServerTime :: ServerTime -> ServerTime
incrementServerTime (ServerTime w) = ServerTime (succ w)

data ServerStore a
  = ServerEmpty ServerTime
  | ServerFull a ServerTime
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ServerStore a)

initialServerStore :: ServerStore a
initialServerStore = ServerEmpty initialServerTime

data SyncRequest a
  = SyncRequestPoll
  | SyncRequestNew a
  | SyncRequestKnown ServerTime
  | SyncRequestKnownButChanged a ServerTime
  | SyncRequestDeletedLocally ServerTime
  deriving (Show, Eq, Generic)

instance Validity a => Validity (SyncRequest a)

data SyncResponse a
  -- | The client and server are fully in sync, and both empty
  --
  -- Nothing needs to be done at the client side.
  = SyncResponseInSyncEmpty
  -- | The client and server are fully in sync.
  --
  -- Nothing needs to be done at the client side.
  | SyncResponseInSyncFull
  -- | The client added an item and server has succesfully been made aware of that.
  --
  -- The client needs to update its server time
  | SyncResponseSuccesfullyAdded ServerTime
  -- | The client changed an item and server has succesfully been made aware of that.
  --
  -- The client needs to update its server time
  | SyncResponseSuccesfullyChanged ServerTime
  -- | The client deleted an item and server has succesfully been made aware of that.
  --
  -- Nothing needs to be done at the client side.
  | SyncResponseSuccesfullyDeleted
  -- | This item has been added on the server side
  --
  -- The client should add it too.
  | SyncResponseNewAtServer a ServerTime
  -- | This item has been modified on the server side.
  --
  -- The client should modify it too.
  | SyncResponseModifiedAtServer a ServerTime
  -- | The item was deleted on the server side
  --
  -- The client should delete it too.
  | SyncResponseDeletedAtServer
  -- | A conflict occurred.
  --
  -- The server and the client both have an item, but it is different.
  -- The server kept its part, the client can either take whatever the server gave them
  -- or deal with the conflict somehow, and then try to re-sync.
  | SyncResponseConflict a -- ^ The item at the server side
  -- | A conflict occurred.
  --
  -- The server has an item but the client does not.
  -- The server kept its part, the client can either take whatever the server gave them
  -- or deal with the conflict somehow, and then try to re-sync.
  | SyncResponseConflictClientDeleted a -- ^ The item at the server side
  -- | A conflict occurred.
  --
  -- The client has a (modified) item but the server does not have any item.
  -- The server left its item deleted, the client can either delete its item too
  -- or deal with the conflict somehow, and then try to re-sync.
  | SyncResponseConflictServerDeleted
  -- | A desync ocurred.
  --
  -- This happens when a server's server time is reset between syncs
  -- or when a client syncs with one server and then with another server.
  | SyncResponseDesynchronised
      ServerTime -- ^ Reported server time at server side
      (Maybe a) -- ^ The item that the server knew about
  deriving (Show, Eq, Generic)

instance Validity a => Validity (SyncResponse a)

makeSyncRequest :: ClientStore a -> SyncRequest a
makeSyncRequest cs =
  case cs of
    ClientEmpty -> SyncRequestPoll
    ClientAdded i -> SyncRequestNew i
    ClientSynced _ st -> SyncRequestKnown st
    ClientSyncedButChanged i st -> SyncRequestKnownButChanged i st
    ClientDeleted st -> SyncRequestDeletedLocally st

data MergeResult a
  = MergeSuccess (ClientStore a)
  | MergeConflict
      a -- ^ The item at the client side
      a -- ^ The item at the server side
  | MergeConflictClientDeleted a -- ^ The item at the server side
  | MergeConflictServerDeleted a -- ^ The item at the client side
  | MergeDesync
      ServerTime -- ^ Server time
      (Maybe a) -- ^ Item at the server side
  | MergeMismatch -- ^ There was a mismatch between the server's response and the client's request.
  deriving (Show, Eq, Generic)

mergeSyncResponseRaw :: ClientStore a -> SyncResponse a -> MergeResult a
mergeSyncResponseRaw cs sr =
  let conflict = cs
      desync = cs
      mismatch = cs
   in case cs of
        ClientEmpty ->
          case sr of
            SyncResponseInSyncEmpty -> MergeSuccess cs
            SyncResponseNewAtServer i st -> MergeSuccess $ ClientSynced i st
            SyncResponseDesynchronised st msi -> MergeDesync st msi
            _ -> MergeMismatch
        ClientAdded ci ->
          case sr of
            SyncResponseSuccesfullyAdded st -> MergeSuccess $ ClientSynced ci st
            SyncResponseConflict si -> MergeConflict ci si
            SyncResponseDesynchronised st msi -> MergeDesync st msi
            _ -> MergeMismatch
        ClientSynced ci ct ->
          case sr of
            SyncResponseInSyncFull -> MergeSuccess $ ClientSynced ci ct
            SyncResponseModifiedAtServer si st -> MergeSuccess $ ClientSynced si st
            SyncResponseDeletedAtServer -> MergeSuccess ClientEmpty
            SyncResponseDesynchronised st msi -> MergeDesync st msi
            _ -> MergeMismatch
        ClientSyncedButChanged ci ct ->
          case sr of
            SyncResponseSuccesfullyChanged st -> MergeSuccess $ ClientSynced ci st
            SyncResponseConflict si -> MergeConflict ci si
            SyncResponseConflictServerDeleted -> MergeConflictServerDeleted ci
            SyncResponseDesynchronised st msi -> MergeDesync st msi
            _ -> MergeMismatch
        ClientDeleted ct ->
          case sr of
            SyncResponseSuccesfullyDeleted -> MergeSuccess ClientEmpty
            SyncResponseConflictClientDeleted si -> MergeConflictClientDeleted si
            SyncResponseDesynchronised st msi -> MergeDesync st msi
            _ -> MergeMismatch

mergeSyncResponseIgnoreProblems :: ClientStore a -> SyncResponse a -> ClientStore a
mergeSyncResponseIgnoreProblems cs = ignoreMergeProblems cs . mergeSyncResponseRaw cs

ignoreMergeProblems :: ClientStore a -> MergeResult a -> ClientStore a
ignoreMergeProblems cs mr =
  case mr of
    MergeSuccess cs' -> cs'
    MergeConflict _ _ -> cs
    MergeConflictServerDeleted _ -> cs
    MergeConflictClientDeleted _ -> cs
    MergeDesync _ _ -> cs
    MergeMismatch -> cs

processServerSync :: ServerStore a -> SyncRequest a -> (SyncResponse a, ServerStore a)
processServerSync store sr =
  case store of
    ServerEmpty st ->
      let t = incrementServerTime st
       in case sr of
            SyncRequestPoll -> (SyncResponseInSyncEmpty, store)
            SyncRequestNew ci -> (SyncResponseSuccesfullyAdded t, ServerFull ci t)
            SyncRequestKnown ct ->
              case compare ct st of
                GT
                  -- The client time is greater than the server time.
                  -- This can only happen if the sync server somehow
                  -- synced this item alread, but then became 'unaware' of it.
                  -- That is impossible in theory.
                  --
                  -- It indicates a desync.
                 -> (SyncResponseDesynchronised st Nothing, store)
                EQ
                  -- The client time is equal to the server time.
                  -- This can only happen if the server somehow didn't update its
                  -- time (that would be a bug), or if a desync happened.
                 -> (SyncResponseDesynchronised st Nothing, store)
                LT
                  -- The client time is less than the server time.
                  -- This indicates that the server synced with another client and was told to
                  -- delete its item.
                  --
                  -- Given that the client indicates that it did not change anything locally,
                  -- the server will just instruct the client to delete its item too.
                  -- No conflict here.
                 -> (SyncResponseDeletedAtServer, store)
            SyncRequestKnownButChanged ci ct ->
              case compare ct st of
                GT
                  -- The client time is greater than the server time.
                  -- This can only happen if the sync server somehow
                  -- synced this item alread, but then became 'unaware' of it.
                  -- That is impossible in theory.
                  --
                  -- It indicates a desync.
                 -> (SyncResponseDesynchronised st Nothing, store)
                EQ
                  -- The client time is equal to the server time.
                  -- This can only happen if the server somehow didn't update its
                  -- time (that would be a bug), or if a desync happened.
                 -> (SyncResponseDesynchronised st Nothing, store)
                LT
                  -- The client time is less than the server time.
                  -- This indicates that the server synced with another client and was told to
                  -- delete its item.
                  --
                  -- Given that the client indicates that it *did* change its item locally,
                  -- there is a conflict.
                 -> (SyncResponseConflictServerDeleted, store)
            SyncRequestDeletedLocally ct -> (SyncResponseDesynchronised st Nothing, store)
    ServerFull si st ->
      let t = incrementServerTime st
       in case sr of
            SyncRequestPoll
              -- The client is empty but the server is not.
              -- This means that the server has synced with another client before,
              -- so we can just send the item to the client.
             -> (SyncResponseNewAtServer si st, store)
            SyncRequestNew ci
              -- The client has a newly added item, so it thought it was empty before that,
              -- but the server has already synced with another client before.
              -- Unless the two items are equal, this indicates a conflict.
              -- The server is always right, so it will remain unmodified.
              -- The client will receive the conflict.
             -> (SyncResponseConflict si, store)
            SyncRequestKnown ct ->
              case compare ct st of
                GT
                  -- The client time is greater than the server time.
                  -- This can only happen if the sync server somehow
                  -- synced this item already, but then became 'unaware' of it.
                  -- That is impossible in theory.
                  --
                  -- It indicates a desync.
                 -> (SyncResponseDesynchronised st (Just si), store)
                EQ
                  -- The client time is equal to the server time.
                  -- The client indicates that the item was not modified at their side.
                  -- This means that the items are in sync.
                  -- (Unless the server somehow modified the item but not its server time,
                  -- which would beconsidered a bug.)
                 -> (SyncResponseInSyncFull, store)
                LT
                  -- The client time is less than the server time
                  -- That means that the server has synced with another client in the meantime.
                  -- Since the client indicates that the item was not modified at their side,
                  -- we can just send it back to the client to have them update their version.
                  -- No conflict here.
                 -> (SyncResponseModifiedAtServer si st, store)
            SyncRequestKnownButChanged ci ct ->
              case compare ct st of
                GT
                  -- The client time is greater than the server time.
                  -- This can only happen if the sync server somehow
                  -- synced this item already, but then became 'unaware' of that synchronisation.
                  -- That is impossible in theory.
                  --
                  -- It indicates a desync.
                 -> (SyncResponseDesynchronised st (Just si), store)
                EQ
                  -- The client time is equal to the server time.
                  -- The client indicates that the item *was* modified at their side.
                  -- This means that the server needs to be updated.
                 -> (SyncResponseSuccesfullyChanged t, ServerFull ci t)
                LT
                  -- The client time is less than the server time
                  -- That means that the server has synced with another client in the meantime.
                  -- Since the client indicates that the item *was* modified at their side,
                  -- there is a conflict.
                 -> (SyncResponseConflict si, store)
            SyncRequestDeletedLocally ct ->
              case compare ct st of
                GT
                  -- The client time is greater than the server time.
                  -- This can only happen if the sync server somehow
                  -- synced this item already, but then became 'unaware' of that synchronisation.
                  -- That is impossible in theory.
                  --
                  -- It indicates a desync.
                 -> (SyncResponseDesynchronised st (Just si), store)
                EQ
                  -- The client time is equal to the server time.
                  -- The client indicates that the item was deleted on their side.
                  -- This means that the server item needs to be deleted as well.
                 -> (SyncResponseSuccesfullyDeleted, ServerEmpty t)
                LT
                  -- The client time is less than the server time
                  -- That means that the server has synced with another client in the meantime.
                  -- Since the client indicates that the item was deleted at their side,
                  -- there is a conflict.
                 -> (SyncResponseConflictClientDeleted si, store)