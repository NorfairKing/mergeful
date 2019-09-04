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

newtype ClientId =
  ClientId
    { unClientId :: Word64
    }
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

instance Validity ClientId

data ClientStore i a =
  ClientStore
    { clientStoreAddedItems :: Map ClientId a
    , clientStoreSyncedItems :: Map i (Timed a)
    , clientStoreSyncedButChangedItems :: Map i (Timed a)
    , clientStoreDeletedItems :: Map i ServerTime
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
    , syncRequestKnownItems :: Map i ServerTime
    , syncRequestKnownButChangedItems :: Map i (Timed a)
    , syncRequestDeletedItems :: Map i ServerTime
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
    , syncResponseClientChanged :: Map i ServerTime
    , syncResponseClientDeleted :: Set i
    , syncResponseServerAdded :: Map i (Timed a)
    , syncResponseServerChanged :: Map i (Timed a)
    , syncResponseServerDeleted :: Set i
    , syncResponseConflicts :: Map i a
    , syncResponseConflictsClientDeleted :: Map i a
    , syncResponseConflictsServerDeleted :: Set i
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
        ItemSyncResponseClientAdded _ -> error "should not happen."
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
processServerSync genId ServerStore {..} SyncRequest {..} = do
  let clientIdentifiedSyncRequests :: Map i (ItemSyncRequest a)
      clientIdentifiedSyncRequests =
        M.unions
          [ M.map ItemSyncRequestKnown syncRequestKnownItems
          , M.map ItemSyncRequestKnownButChanged syncRequestKnownButChangedItems
          , M.map ItemSyncRequestDeletedLocally syncRequestDeletedItems
          ]
      serverIdentifiedItems :: Map i (ServerItem a)
      serverIdentifiedItems = M.map ServerFull serverStoreItems
      thesePairs :: Map i (These (ServerItem a) (ItemSyncRequest a))
      thesePairs = unionThese serverIdentifiedItems clientIdentifiedSyncRequests
      requestPairs :: Map i (ServerItem a, ItemSyncRequest a)
      requestPairs = M.map (fromThese ServerEmpty ItemSyncRequestPoll) thesePairs
      unidentifedPairs :: Map ClientId (ServerItem a, ItemSyncRequest a)
      unidentifedPairs = M.map (\a -> (ServerEmpty, ItemSyncRequestNew a)) syncRequestNewItems
      unidentifedResults :: Map ClientId (ItemSyncResponse a, ServerItem a)
      unidentifedResults = M.map (uncurry processServerItemSync) unidentifedPairs
  generatedResults <-
    fmap M.fromList $
    forM (M.toList unidentifedResults) $ \(int, r) -> do
      uuid <- genId
      pure ((uuid, int), r)
  let identifiedResults :: Map i (ItemSyncResponse a, ServerItem a)
      identifiedResults = M.map (uncurry processServerItemSync) requestPairs
      allResults :: Map (Identifier i) (ItemSyncResponse a, ServerItem a)
      allResults =
        M.union
          (M.mapKeys OnlyServer identifiedResults)
          (M.mapKeys (uncurry BothServerAndClient) generatedResults)
      resp :: SyncResponse i a
      resp =
        M.foldlWithKey
          (\sr cid (isr, _) -> addToSyncResponse sr cid isr)
          emptySyncResponse
          allResults
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
  pure (resp, ServerStore newStore)

unionThese :: Ord k => Map k a -> Map k b -> Map k (These a b)
unionThese m1 m2 = M.unionWith go (M.map This m1) (M.map That m2)
  where
    go (This a) (That b) = These a b
    go _ _ = error "should not happen."

distinct :: Eq a => [a] -> Bool
distinct ls = nub ls == ls
