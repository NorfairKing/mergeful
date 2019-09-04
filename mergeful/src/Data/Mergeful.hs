{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A way to synchronise a single item with safe merge conflicts.
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
-- The client starts with an 'initialClientStore'.
--
-- * The client produces a 'SyncRequest' with 'makeSyncRequest'.
-- * The client sends that request to the central server and gets a 'SyncResponse'.
-- * The client then updates its local store with 'mergeSyncResponseIgnoreProblems'.
--
--
-- = The central server should operate as follows:
--
-- The server starts with an 'initialServerStore'.
--
-- * The server accepts a 'SyncRequest'.
-- * The server performs operations according to the functionality of 'processServerSync'.
-- * The server respons with a 'SyncResponse'.
--
--
-- WARNING:
-- This whole approach can break down if a server resets its server times
-- or if a client syncs with two different servers using the same server times.
module Data.Mergeful
  ( initialClientStore
  , addItemToClientStore
  , initialSyncRequest
  , makeSyncRequest
  , mergeSyncResponseIgnoreProblems
  -- * Server side
  , initialServerStore
  , processServerSync
  -- * Types, for reference
  , ClientStore(..)
  , SyncRequest(..)
  , SyncResponse(..)
  , emptySyncResponse
  , ServerStore(..)
  , ClientId(..)
  -- * Utility functions for implementing client-side merging
  , mergeAddedItems
  , mergeSyncedButChangedItems
  , mergeDeletedItems
  -- * Utility functions for implementing server-side responding
  , addToSyncResponse
  ) where

import GHC.Generics (Generic)

import Control.Monad
import Data.Aeson
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.These
import Data.Validity
import Data.Validity.Containers ()
import Data.Word

import Data.Mergeful.Item
import Data.Mergeful.Timed

-- | A Client-side identifier for items.
--
-- These only need to be unique at the client.
newtype ClientId =
  ClientId
    { unClientId :: Word64
    }
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

instance Validity ClientId

data ClientStore i a =
  ClientStore
    { clientStoreAddedItems :: Map ClientId a
      -- ^ These items are new locally but have not been synced to the server yet.
    , clientStoreSyncedItems :: Map i (Timed a)
      -- ^ These items have been synced at their respective 'ServerTime's.
    , clientStoreSyncedButChangedItems :: Map i (Timed a)
      -- ^ These items have been synced at their respective 'ServerTime's
      -- but modified locally since then.
    , clientStoreDeletedItems :: Map i ServerTime
      -- ^ These items have been deleted locally after they were synced
      -- but the server has not been notified of that yet.
    }
  deriving (Show, Eq, Generic)

instance (Validity i, Ord i, Validity a) => Validity (ClientStore i a) where
  validate cs@ClientStore {..} =
    mconcat
      [ genericValidate cs
      , declare "There are no duplicate IDs" $
        distinct $
        concat $
        [ M.keys clientStoreSyncedItems
        , M.keys clientStoreSyncedButChangedItems
        , M.keys clientStoreDeletedItems
        ]
      ]

instance (Ord i, FromJSONKey i, FromJSON a) => FromJSON (ClientStore i a) where
  parseJSON =
    withObject "ClientStore" $ \o ->
      ClientStore <$> o .:? "added" .!= M.empty <*> o .:? "synced" .!= M.empty <*>
      o .:? "changed" .!= M.empty <*>
      o .:? "deleted" .!= M.empty

instance (ToJSONKey i, ToJSON a) => ToJSON (ClientStore i a) where
  toJSON ClientStore {..} =
    object $
    catMaybes $
    [ jNull "added" clientStoreAddedItems
    , jNull "synced" clientStoreSyncedItems
    , jNull "changed" clientStoreSyncedButChangedItems
    , jNull "deleted" clientStoreDeletedItems
    ]

-- | A client store to start with.
--
-- This store contains no items.
initialClientStore :: ClientStore i a
initialClientStore =
  ClientStore
    { clientStoreAddedItems = M.empty
    , clientStoreSyncedItems = M.empty
    , clientStoreSyncedButChangedItems = M.empty
    , clientStoreDeletedItems = M.empty
    }

-- | Add an item to a client store as an added item.
--
-- This will take care of the uniqueness constraint of the 'ClientId's in the map.
addItemToClientStore :: a -> ClientStore i a -> ClientStore i a
addItemToClientStore a cs =
  let oldAddedItems = clientStoreAddedItems cs
      newAddedItems =
        let newKey =
              ClientId $
              if M.null oldAddedItems
                then 0
                else let (ClientId k, _) = M.findMax oldAddedItems
                      in succ k
         in M.insert newKey a oldAddedItems
   in cs {clientStoreAddedItems = newAddedItems}

newtype ServerStore i a =
  ServerStore
    { serverStoreItems :: Map i (Timed a)
      -- ^ A map of items, named using an 'i', together with the 'ServerTime' at which
      -- they were last synced.
    }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance (Validity i, Ord i, Validity a) => Validity (ServerStore i a)

-- | A server store to start with
--
-- This store contains no items.
initialServerStore :: ServerStore i a
initialServerStore = ServerStore {serverStoreItems = M.empty}

data SyncRequest i a =
  SyncRequest
    { syncRequestNewItems :: Map ClientId a
      -- ^ These items are new locally but have not been synced to the server yet.
    , syncRequestKnownItems :: Map i ServerTime
      -- ^ These items have been synced at their respective 'ServerTime's.
    , syncRequestKnownButChangedItems :: Map i (Timed a)
      -- ^ These items have been synced at their respective 'ServerTime's
      -- but modified locally since then.
    , syncRequestDeletedItems :: Map i ServerTime
      -- ^ These items have been deleted locally after they were synced
      -- but the server has not been notified of that yet.
    }
  deriving (Show, Eq, Generic)

instance (Validity i, Ord i, Validity a) => Validity (SyncRequest i a) where
  validate sr@SyncRequest {..} =
    mconcat
      [ genericValidate sr
      , declare "There are no duplicate IDs" $
        distinct $
        concat $
        [ M.keys syncRequestKnownItems
        , M.keys syncRequestKnownButChangedItems
        , M.keys syncRequestDeletedItems
        ]
      ]

instance (Ord i, FromJSONKey i, FromJSON a) => FromJSON (SyncRequest i a) where
  parseJSON =
    withObject "SyncRequest" $ \o ->
      SyncRequest <$> o .:? "new" .!= M.empty <*> o .:? "synced" .!= M.empty <*>
      o .:? "changed" .!= M.empty <*>
      o .:? "deleted" .!= M.empty

instance (ToJSONKey i, ToJSON a) => ToJSON (SyncRequest i a) where
  toJSON SyncRequest {..} =
    object $
    catMaybes $
    [ jNull "new" syncRequestNewItems
    , jNull "synced" syncRequestKnownItems
    , jNull "changed" syncRequestKnownButChangedItems
    , jNull "deleted" syncRequestDeletedItems
    ]

-- | An intial 'SyncRequest' to start with.
--
-- It just asks the server to send over whatever it knows.
initialSyncRequest :: SyncRequest i a
initialSyncRequest =
  SyncRequest
    { syncRequestNewItems = M.empty
    , syncRequestKnownItems = M.empty
    , syncRequestKnownButChangedItems = M.empty
    , syncRequestDeletedItems = M.empty
    }

data SyncResponse i a =
  SyncResponse
    { syncResponseClientAdded :: Map ClientId (i, ServerTime)
      -- ^ The client added these items and server has succesfully been made aware of that.
      --
      -- The client needs to update their server times
    , syncResponseClientChanged :: Map i ServerTime
      -- ^ The client changed these items and server has succesfully been made aware of that.
      --
      -- The client needs to update their server times
    , syncResponseClientDeleted :: Set i
      -- ^ The client deleted these items and server has succesfully been made aware of that.
      --
      -- The client can delete them from its deleted items
    , syncResponseServerAdded :: Map i (Timed a)
      -- ^ These items have been added on the server side
      --
      -- The client should add them too.
    , syncResponseServerChanged :: Map i (Timed a)
      -- ^ These items have been modified on the server side.
      --
      -- The client should modify them too.
    , syncResponseServerDeleted :: Set i
      -- ^ These items were deleted on the server side
      --
      -- The client should delete them too
    , syncResponseConflicts :: Map i a
      -- ^ These are conflicts where the server and the client both have an item, but it is different.
      --
      -- The server kept its part of each, the client can either take whatever the server gave them
      -- or deal with the conflicts somehow, and then try to re-sync.
    , syncResponseConflictsClientDeleted :: Map i a
      -- ^ These are conflicts where the server has an item but the client does not.
      --
      -- The server kept its item, the client can either take whatever the server gave them
      -- or deal with the conflicts somehow, and then try to re-sync.
    , syncResponseConflictsServerDeleted :: Set i
      -- ^ These are conflicts where the server has no item but the client has a modified item.
      --
      -- The server left its item deleted, the client can either delete its items too
      -- or deal with the conflicts somehow, and then try to re-sync.
    }
  deriving (Show, Eq, Generic)

instance (Validity i, Ord i, Validity a) => Validity (SyncResponse i a) where
  validate sr@SyncResponse {..} =
    mconcat
      [ genericValidate sr
      , declare "There are no duplicate IDs" $
        distinct $
        concat $
        [ map (\(_, (i, _)) -> i) $ M.toList syncResponseClientAdded
        , M.keys syncResponseClientChanged
        , S.toList syncResponseClientDeleted
        , M.keys syncResponseServerAdded
        , M.keys syncResponseServerChanged
        , S.toList syncResponseServerDeleted
        , M.keys syncResponseConflicts
        , M.keys syncResponseConflictsClientDeleted
        , S.toList syncResponseConflictsServerDeleted
        ]
      ]

instance (Ord i, FromJSON i, FromJSONKey i, FromJSON a) => FromJSON (SyncResponse i a) where
  parseJSON =
    withObject "SyncResponse" $ \o ->
      SyncResponse <$> o .:? "client-added" .!= M.empty <*> o .:? "client-changed" .!= M.empty <*>
      o .:? "client-deleted" .!= S.empty <*>
      o .:? "server-added" .!= M.empty <*>
      o .:? "server-changed" .!= M.empty <*>
      o .:? "server-deleted" .!= S.empty <*>
      o .:? "conflict" .!= M.empty <*>
      o .:? "conflict-client-deleted" .!= M.empty <*>
      o .:? "conflict-server-deleted" .!= S.empty

instance (ToJSON i, ToJSONKey i, ToJSON a) => ToJSON (SyncResponse i a) where
  toJSON SyncResponse {..} =
    object $
    catMaybes $
    [ jNull "client-added" syncResponseClientAdded
    , jNull "client-changed" syncResponseClientChanged
    , jNull "client-deleted" syncResponseClientDeleted
    , jNull "server-added" syncResponseServerAdded
    , jNull "server-changed" syncResponseServerChanged
    , jNull "server-deleted" syncResponseServerDeleted
    , jNull "conflict" syncResponseConflicts
    , jNull "conflict-client-deleted" syncResponseConflictsClientDeleted
    , jNull "conflict-server-deleted" syncResponseConflictsServerDeleted
    ]

emptySyncResponse :: SyncResponse i a
emptySyncResponse =
  SyncResponse
    { syncResponseClientAdded = M.empty
    , syncResponseClientChanged = M.empty
    , syncResponseClientDeleted = S.empty
    , syncResponseServerAdded = M.empty
    , syncResponseServerChanged = M.empty
    , syncResponseServerDeleted = S.empty
    , syncResponseConflicts = M.empty
    , syncResponseConflictsClientDeleted = M.empty
    , syncResponseConflictsServerDeleted = S.empty
    }

jNull :: (Foldable f, ToJSON (f b)) => Text -> f b -> Maybe (Text, Value)
jNull n s =
  if null s
    then Nothing
    else Just $ n .= s

-- | Produce an 'SyncRequest' from a 'ClientStore'.
--
-- Send this to the server for synchronisation.
makeSyncRequest :: ClientStore i a -> SyncRequest i a
makeSyncRequest ClientStore {..} =
  SyncRequest
    { syncRequestNewItems = clientStoreAddedItems
    , syncRequestKnownItems = M.map timedTime clientStoreSyncedItems
    , syncRequestKnownButChangedItems = clientStoreSyncedButChangedItems
    , syncRequestDeletedItems = clientStoreDeletedItems
    }

-- | Merge an 'SyncResponse' into the current 'ClientStore'.
--
-- This function ignores any problems that may occur.
-- In the case of a conclict, it will just not update the client item.
-- The next sync request will then produce a conflict again.
mergeSyncResponseIgnoreProblems :: Ord i => ClientStore i a -> SyncResponse i a -> ClientStore i a
mergeSyncResponseIgnoreProblems cs SyncResponse {..} =
  let (addedItemsLeftovers, newSyncedItems) =
        mergeAddedItems (clientStoreAddedItems cs) syncResponseClientAdded
      (syncedButNotChangedLeftovers, newModifiedItems) =
        mergeSyncedButChangedItems (clientStoreSyncedButChangedItems cs) syncResponseClientChanged
      deletedItemsLeftovers =
        mergeDeletedItems (clientStoreDeletedItems cs) syncResponseClientDeleted
      synced =
        M.unions
          [ newSyncedItems
          , syncResponseServerAdded
          , syncResponseServerChanged
          , newModifiedItems
          , clientStoreSyncedItems cs
          ]
   in ClientStore
        { clientStoreAddedItems = addedItemsLeftovers
        , clientStoreSyncedButChangedItems = syncedButNotChangedLeftovers `M.difference` synced
        , clientStoreDeletedItems = deletedItemsLeftovers `M.difference` synced
        , clientStoreSyncedItems =
            synced `M.difference` (M.fromSet (const ()) syncResponseServerDeleted)
        }

mergeAddedItems ::
     forall i a. Ord i
  => Map ClientId a
  -> Map ClientId (i, ServerTime)
  -> (Map ClientId a, Map i (Timed a))
mergeAddedItems local added = M.foldlWithKey go (M.empty, M.empty) local
  where
    go :: (Map ClientId a, Map i (Timed a)) -> ClientId -> a -> (Map ClientId a, Map i (Timed a))
    go (as, m) i a =
      case M.lookup i added of
        Nothing -> (M.insert i a as, m)
        Just (k, st) -> (as, M.insert k (Timed {timedValue = a, timedTime = st}) m)

mergeSyncedButChangedItems ::
     forall i a. Ord i
  => Map i (Timed a)
  -> Map i ServerTime
  -> (Map i (Timed a), Map i (Timed a))
mergeSyncedButChangedItems local changed = M.foldlWithKey go (M.empty, M.empty) local
  where
    go :: (Map i (Timed a), Map i (Timed a)) -> i -> Timed a -> (Map i (Timed a), Map i (Timed a))
    go (m1, m2) k t =
      case M.lookup k changed of
        Nothing -> (M.insert k t m1, m2)
        Just st' -> (m1, M.insert k (t {timedTime = st'}) m2)

mergeDeletedItems :: Ord i => Map i b -> Set i -> (Map i b)
mergeDeletedItems m s = m `M.difference` M.fromSet (const ()) s

addToSyncResponse ::
     Ord i => SyncResponse i a -> Identifier i -> ItemSyncResponse a -> SyncResponse i a
addToSyncResponse sr cid isr =
  case cid of
    BothServerAndClient i int ->
      case isr of
        ItemSyncResponseClientAdded st ->
          sr {syncResponseClientAdded = M.insert int (i, st) $ syncResponseClientAdded sr}
        _ -> error "should not happen"
    OnlyServer i ->
      case isr of
        ItemSyncResponseInSyncEmpty -> sr
        ItemSyncResponseInSyncFull -> sr
        ItemSyncResponseClientAdded _ -> sr -- Should not happen.
        ItemSyncResponseClientChanged st ->
          sr {syncResponseClientChanged = M.insert i st $ syncResponseClientChanged sr}
        ItemSyncResponseClientDeleted ->
          sr {syncResponseClientDeleted = S.insert i $ syncResponseClientDeleted sr}
        ItemSyncResponseServerAdded t ->
          sr {syncResponseServerAdded = M.insert i t $ syncResponseServerAdded sr}
        ItemSyncResponseServerChanged t ->
          sr {syncResponseServerChanged = M.insert i t $ syncResponseServerChanged sr}
        ItemSyncResponseServerDeleted ->
          sr {syncResponseServerDeleted = S.insert i $ syncResponseServerDeleted sr}
        ItemSyncResponseConflict a ->
          sr {syncResponseConflicts = M.insert i a $ syncResponseConflicts sr}
        ItemSyncResponseConflictClientDeleted a ->
          sr
            { syncResponseConflictsClientDeleted =
                M.insert i a $ syncResponseConflictsClientDeleted sr
            }
        ItemSyncResponseConflictServerDeleted ->
          sr
            { syncResponseConflictsServerDeleted =
                S.insert i $ syncResponseConflictsServerDeleted sr
            }

data Identifier i
  = OnlyServer i
  | BothServerAndClient i ClientId
  deriving (Show, Eq, Ord, Generic)

instance Validity i => Validity (Identifier i)

-- | Serve an 'SyncRequest' using the current 'ServerStore', producing an 'SyncResponse' and a new 'ServerStore'.
processServerSync ::
     forall i a m. (Ord i, Monad m)
  => m i -- ^ The action that is guaranteed to generate unique identifiers
  -> ServerStore i a
  -> SyncRequest i a
  -> m (SyncResponse i a, ServerStore i a)
processServerSync genId ServerStore {..} sr@SyncRequest {..}
      -- Make tuples of requests for all of the items that only had a client identifier.
 = do
  let unidentifedPairs :: Map ClientId (ServerItem a, ItemSyncRequest a)
      unidentifedPairs = M.map (\a -> (ServerEmpty, ItemSyncRequestNew a)) syncRequestNewItems
      -- Make tuples of results for each of the unidentifier tuples.
      unidentifedResults :: Map ClientId (ItemSyncResponse a, ServerItem a)
      unidentifedResults = M.map (uncurry processServerItemSync) unidentifedPairs
  generatedResults <- generateIdentifiersFor genId unidentifedResults
      -- Gather the items that had a server identifier already.
  let clientIdentifiedSyncRequests :: Map i (ItemSyncRequest a)
      clientIdentifiedSyncRequests = identifiedItemSyncRequests sr
      -- Make 'ServerItem's for each of the items on the server side
      serverIdentifiedItems :: Map i (ServerItem a)
      serverIdentifiedItems = M.map ServerFull serverStoreItems
      -- Match up client items with server items by their id.
      thesePairs :: Map i (These (ServerItem a) (ItemSyncRequest a))
      thesePairs = unionTheseMaps serverIdentifiedItems clientIdentifiedSyncRequests
      -- Make tuples of server 'ServerItem's and 'ItemSyncRequest's for each of the items with an id
      requestPairs :: Map i (ServerItem a, ItemSyncRequest a)
      requestPairs = M.map (fromThese ServerEmpty ItemSyncRequestPoll) thesePairs
      -- Make tuples of results for each of the tuplus that had a server identifier.
      identifiedResults :: Map i (ItemSyncResponse a, ServerItem a)
      identifiedResults = M.map (uncurry processServerItemSync) requestPairs
      -- Put together the results together
  let allResults :: Map (Identifier i) (ItemSyncResponse a, ServerItem a)
      allResults =
        M.union
          (M.mapKeys OnlyServer identifiedResults)
          (M.mapKeys (uncurry BothServerAndClient) generatedResults)
  pure $ produceSyncResults allResults

identifiedItemSyncRequests :: Ord i => SyncRequest i a -> Map i (ItemSyncRequest a)
identifiedItemSyncRequests SyncRequest {..} =
  M.unions
    [ M.map ItemSyncRequestKnown syncRequestKnownItems
    , M.map ItemSyncRequestKnownButChanged syncRequestKnownButChangedItems
    , M.map ItemSyncRequestDeletedLocally syncRequestDeletedItems
    ]

generateIdentifiersFor ::
     (Ord i, Monad m)
  => m i
  -> Map ClientId (ItemSyncResponse a, ServerItem a)
  -> m (Map (i, ClientId) (ItemSyncResponse a, ServerItem a))
generateIdentifiersFor genId unidentifedResults =
  fmap M.fromList $
  forM (M.toList unidentifedResults) $ \(int, r) -> do
    uuid <- genId
    pure ((uuid, int), r)

produceSyncResults ::
     forall i a. Ord i
  => Map (Identifier i) (ItemSyncResponse a, ServerItem a)
  -> (SyncResponse i a, ServerStore i a)
produceSyncResults allResults
      -- Produce a sync response
 =
  let resp :: SyncResponse i a
      resp =
        M.foldlWithKey
          (\sr cid (isr, _) -> addToSyncResponse sr cid isr)
          emptySyncResponse
          allResults
      -- Produce a new server store
      newStore :: Map i (Timed a)
      newStore =
        M.mapMaybe
          (\si ->
             case si of
               ServerEmpty -> Nothing
               ServerFull t -> Just t) $
        M.map snd $
        M.mapKeys
          (\cid ->
             case cid of
               OnlyServer i -> i
               BothServerAndClient i _ -> i)
          allResults
      -- return them both.
   in (resp, ServerStore newStore)

unionTheseMaps :: Ord k => Map k a -> Map k b -> Map k (These a b)
unionTheseMaps m1 m2 = M.unionWith go (M.map This m1) (M.map That m2)
  where
    go (This a) (That b) = These a b
    go _ _ = error "should not happen."

distinct :: Eq a => [a] -> Bool
distinct ls = nub ls == ls
