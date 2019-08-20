{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

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
  ) where

import GHC.Generics (Generic)

import Data.Validity
import Data.Word

data ClientItem a
  -- | There is no item on the client side
  = ClientEmpty
  -- | There is is an item but the server is not aware of it yet.
  | ClientAdded a
  -- | There is is an item and it has been synced with the server.
  | ClientItemSynced a ServerTime
  -- | There is is an item and it has been synced with the server, but it has since been modified.
  | ClientItemSyncedButChanged a ServerTime
  -- | There was an item, and it has been deleted locally, but the server has not been made aware of this.
  | ClientDeleted ServerTime
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ClientItem a)

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

data ServerItem a
  = ServerEmpty ServerTime
  | ServerFull a ServerTime
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ServerItem a)

initialServerItem :: ServerItem a
initialServerItem = ServerEmpty initialServerTime

data ItemSyncRequest a
  -- | There is no item locally
  = ItemSyncRequestPoll
  -- | There is an item locally that hasn't been synced to the server yet.
  | ItemSyncRequestNew a
  -- | There is an item locally that was synced at the given 'ServerTime'
  | ItemSyncRequestKnown ServerTime
  -- | There is an item locally that was synced at the given 'ServerTime'
  -- but it has been changed since then.
  | ItemSyncRequestKnownButChanged a ServerTime
  -- | There was an item locally that has been deleted but the
  -- deletion wasn't synced to the server yet.
  | ItemSyncRequestDeletedLocally ServerTime
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ItemSyncRequest a)

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
  | ItemSyncResponseSuccesfullyAdded ServerTime
  -- | The client changed an item and server has succesfully been made aware of that.
  --
  -- The client needs to update its server time
  | ItemSyncResponseSuccesfullyChanged ServerTime
  -- | The client deleted an item and server has succesfully been made aware of that.
  --
  -- Nothing needs to be done at the client side.
  | ItemSyncResponseSuccesfullyDeleted
  -- | This item has been added on the server side
  --
  -- The client should add it too.
  | ItemSyncResponseNewAtServer a ServerTime
  -- | This item has been modified on the server side.
  --
  -- The client should modify it too.
  | ItemSyncResponseModifiedAtServer a ServerTime
  -- | The item was deleted on the server side
  --
  -- The client should delete it too.
  | ItemSyncResponseDeletedAtServer
  -- | A conflict occurred.
  --
  -- The server and the client both have an item, but it is different.
  -- The server kept its part, the client can either take whatever the server gave them
  -- or deal with the conflict somehow, and then try to re-sync.
  | ItemSyncResponseConflict a -- ^ The item at the server side
  -- | A conflict occurred.
  --
  -- The server has an item but the client does not.
  -- The server kept its part, the client can either take whatever the server gave them
  -- or deal with the conflict somehow, and then try to re-sync.
  | ItemSyncResponseConflictClientDeleted a -- ^ The item at the server side
  -- | A conflict occurred.
  --
  -- The client has a (modified) item but the server does not have any item.
  -- The server left its item deleted, the client can either delete its item too
  -- or deal with the conflict somehow, and then try to re-sync.
  | ItemSyncResponseConflictServerDeleted
  -- | A desync ocurred.
  --
  -- This happens when a server's server time is reset between syncs
  -- or when a client syncs with one server and then with another server.
  | ItemSyncResponseDesynchronised
      ServerTime -- ^ Reported server time at server side
      (Maybe a) -- ^ The item that the server knew about
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ItemSyncResponse a)

makeItemSyncRequest :: ClientItem a -> ItemSyncRequest a
makeItemSyncRequest cs =
  case cs of
    ClientEmpty -> ItemSyncRequestPoll
    ClientAdded i -> ItemSyncRequestNew i
    ClientItemSynced _ st -> ItemSyncRequestKnown st
    ClientItemSyncedButChanged i st -> ItemSyncRequestKnownButChanged i st
    ClientDeleted st -> ItemSyncRequestDeletedLocally st

data MergeResult a
  -- | The merger went succesfully, no conflicts or desyncs
  = MergeSuccess (ClientItem a)
  -- | There was a merge conflict. The server and client had different, conflicting versions.
  | MergeConflict
      a -- ^ The item at the client side
      a -- ^ The item at the server side
  -- | There was a merge conflict. The client had deleted the item while the server had modified it.
  | MergeConflictClientDeleted a -- ^ The item at the server side
  -- | There was a merge conflict. The server had deleted the item while the client had modified it.
  | MergeConflictServerDeleted a -- ^ The item at the client side
  -- | A desync was detected.
  --
  -- This only occurs if the server's time has been reset
  -- or if a client syncs with multiple servers with the same server time.
  | MergeDesync
      ServerTime -- ^ Server time
      (Maybe a) -- ^ Item at the server side
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
        ItemSyncResponseNewAtServer i st -> MergeSuccess $ ClientItemSynced i st
        ItemSyncResponseDesynchronised st msi -> MergeDesync st msi
        _ -> MergeMismatch
    ClientAdded ci ->
      case sr of
        ItemSyncResponseSuccesfullyAdded st -> MergeSuccess $ ClientItemSynced ci st
        ItemSyncResponseConflict si -> MergeConflict ci si
        ItemSyncResponseDesynchronised st msi -> MergeDesync st msi
        _ -> MergeMismatch
    ClientItemSynced ci ct ->
      case sr of
        ItemSyncResponseInSyncFull -> MergeSuccess $ ClientItemSynced ci ct
        ItemSyncResponseModifiedAtServer si st -> MergeSuccess $ ClientItemSynced si st
        ItemSyncResponseDeletedAtServer -> MergeSuccess ClientEmpty
        ItemSyncResponseDesynchronised st msi -> MergeDesync st msi
        _ -> MergeMismatch
    ClientItemSyncedButChanged ci _ ->
      case sr of
        ItemSyncResponseSuccesfullyChanged st -> MergeSuccess $ ClientItemSynced ci st
        ItemSyncResponseConflict si -> MergeConflict ci si
        ItemSyncResponseConflictServerDeleted -> MergeConflictServerDeleted ci
        ItemSyncResponseDesynchronised st msi -> MergeDesync st msi
        _ -> MergeMismatch
    ClientDeleted _ ->
      case sr of
        ItemSyncResponseSuccesfullyDeleted -> MergeSuccess ClientEmpty
        ItemSyncResponseConflictClientDeleted si -> MergeConflictClientDeleted si
        ItemSyncResponseDesynchronised st msi -> MergeDesync st msi
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
    ServerEmpty st ->
      let t = incrementServerTime st
       in case sr of
            ItemSyncRequestPoll -> (ItemSyncResponseInSyncEmpty, store)
            ItemSyncRequestNew ci -> (ItemSyncResponseSuccesfullyAdded t, ServerFull ci t)
            ItemSyncRequestKnown ct ->
              case compare ct st of
                GT
                  -- The client time is greater than the server time.
                  -- This can only happen if the sync server somehow
                  -- synced this item alread, but then became 'unaware' of it.
                  -- That is impossible in theory.
                  --
                  -- It indicates a desync.
                 -> (ItemSyncResponseDesynchronised st Nothing, store)
                EQ
                  -- The client time is equal to the server time.
                  -- This can only happen if the server somehow didn't update its
                  -- time (that would be a bug), or if a desync happened.
                 -> (ItemSyncResponseDesynchronised st Nothing, store)
                LT
                  -- The client time is less than the server time.
                  -- This indicates that the server synced with another client and was told to
                  -- delete its item.
                  --
                  -- Given that the client indicates that it did not change anything locally,
                  -- the server will just instruct the client to delete its item too.
                  -- No conflict here.
                 -> (ItemSyncResponseDeletedAtServer, store)
            ItemSyncRequestKnownButChanged _ ct ->
              case compare ct st of
                GT
                  -- The client time is greater than the server time.
                  -- This can only happen if the sync server somehow
                  -- synced this item already, but then became 'unaware' of it.
                  -- That is impossible in theory.
                  --
                  -- It indicates a desync.
                 -> (ItemSyncResponseDesynchronised st Nothing, store)
                EQ
                  -- The client time is equal to the server time.
                  -- This can only happen if the server somehow didn't update its
                  -- time (that would be a bug), or if a desync happened.
                 -> (ItemSyncResponseDesynchronised st Nothing, store)
                LT
                  -- The client time is less than the server time.
                  -- This indicates that the server synced with another client and was told to
                  -- delete its item.
                  --
                  -- Given that the client indicates that it *did* change its item locally,
                  -- there is a conflict.
                 -> (ItemSyncResponseConflictServerDeleted, store)
            ItemSyncRequestDeletedLocally ct ->
              case compare ct st of
                GT
                  -- The client time is greater than the server time.
                  -- This can only happen if the sync server somehow
                  -- synced this item already, but then became 'unaware' of it.
                  -- That is impossible in theory.
                  --
                  -- It indicates a desync.
                 -> (ItemSyncResponseDesynchronised st Nothing, store)
                EQ
                  -- The client time is equal to the server time.
                  -- This means that the client was in sync with the server,
                  -- and is now telling the server to delete this item,
                  -- but the item isn't there on the server side anyway.
                  --
                  -- This indicates a desync
                 -> (ItemSyncResponseDesynchronised st Nothing, store)
                LT
                  -- The client time is less than the server time
                  -- This means that the server synced with another client,
                  -- was instructed to delete its item by that client,
                  -- and is now being told to delete its item again.
                  --
                  -- That's fine, it will just remain deleted.
                  -- No conflict here
                 -> (ItemSyncResponseSuccesfullyDeleted, store)
    ServerFull si st ->
      let t = incrementServerTime st
       in case sr of
            ItemSyncRequestPoll
              -- The client is empty but the server is not.
              -- This means that the server has synced with another client before,
              -- so we can just send the item to the client.
             -> (ItemSyncResponseNewAtServer si st, store)
            ItemSyncRequestNew _
              -- The client has a newly added item, so it thought it was empty before that,
              -- but the server has already synced with another client before.
              -- This indicates a conflict.
              -- The server is always right, so the item at the server will remain unmodified.
              -- The client will receive the conflict.
             -> (ItemSyncResponseConflict si, store)
            ItemSyncRequestKnown ct ->
              case compare ct st of
                GT
                  -- The client time is greater than the server time.
                  -- This can only happen if the sync server somehow
                  -- synced this item already, but then became 'unaware' of it.
                  -- That is impossible in theory.
                  --
                  -- It indicates a desync.
                 -> (ItemSyncResponseDesynchronised st (Just si), store)
                EQ
                  -- The client time is equal to the server time.
                  -- The client indicates that the item was not modified at their side.
                  -- This means that the items are in sync.
                  -- (Unless the server somehow modified the item but not its server time,
                  -- which would beconsidered a bug.)
                 -> (ItemSyncResponseInSyncFull, store)
                LT
                  -- The client time is less than the server time
                  -- That means that the server has synced with another client in the meantime.
                  -- Since the client indicates that the item was not modified at their side,
                  -- we can just send it back to the client to have them update their version.
                  -- No conflict here.
                 -> (ItemSyncResponseModifiedAtServer si st, store)
            ItemSyncRequestKnownButChanged ci ct ->
              case compare ct st of
                GT
                  -- The client time is greater than the server time.
                  -- This can only happen if the sync server somehow
                  -- synced this item already, but then became 'unaware' of that synchronisation.
                  -- That is impossible in theory.
                  --
                  -- It indicates a desync.
                 -> (ItemSyncResponseDesynchronised st (Just si), store)
                EQ
                  -- The client time is equal to the server time.
                  -- The client indicates that the item *was* modified at their side.
                  -- This means that the server needs to be updated.
                 -> (ItemSyncResponseSuccesfullyChanged t, ServerFull ci t)
                LT
                  -- The client time is less than the server time
                  -- That means that the server has synced with another client in the meantime.
                  -- Since the client indicates that the item *was* modified at their side,
                  -- there is a conflict.
                 -> (ItemSyncResponseConflict si, store)
            ItemSyncRequestDeletedLocally ct ->
              case compare ct st of
                GT
                  -- The client time is greater than the server time.
                  -- This can only happen if the sync server somehow
                  -- synced this item already, but then became 'unaware' of that synchronisation.
                  -- That is impossible in theory.
                  --
                  -- It indicates a desync.
                 -> (ItemSyncResponseDesynchronised st (Just si), store)
                EQ
                  -- The client time is equal to the server time.
                  -- The client indicates that the item was deleted on their side.
                  -- This means that the server item needs to be deleted as well.
                 -> (ItemSyncResponseSuccesfullyDeleted, ServerEmpty t)
                LT
                  -- The client time is less than the server time
                  -- That means that the server has synced with another client in the meantime.
                  -- Since the client indicates that the item was deleted at their side,
                  -- there is a conflict.
                 -> (ItemSyncResponseConflictClientDeleted si, store)
