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
-- A central server should operate as follows:
--
-- * The server accepts a 'SyncRequest'.
-- * The server performs operations according to the functionality of 'processServerSync'.
-- * The server respons with a 'SyncResponse'.
--
--
-- A client should operate as follows:
--
-- * The client produces a 'SyncRequest' with 'makeSyncRequest'.
-- * The client sends that request to the central server and gets a 'SyncResponse'.
-- * The client then updates its local store with 'mergeSyncResponseIgnoreProblems'.
--
-- -- WARNING:
-- This whole approach can break down if a server resets its server times
-- or if a client syncs with two different servers using the same server times.
module Data.Mergeful
  ( ClientStore(..)
  , emptyClientStore
  , SyncRequest(..)
  , makeSyncRequest
  , SyncResponse(..)
  , emptySyncResponse
  , mergeSyncResponseIgnoreProblems
  , addedItemsIntmap
  , mergeAddedItems
  , mergeSyncedButChangedItems
  , mergeDeletedItems
  , ServerStore(..)
  , emptyServerStore
  , processServerSync
  , Timed(..)
  , ServerTime
  , initialServerTime
  , incrementServerTime
  ) where

import GHC.Generics (Generic)

import Control.Monad
import Data.Aeson
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.These
import Data.Validity
import Data.Validity.Containers ()

import Data.Mergeful.Item

data ClientStore i a =
  ClientStore
    { clientStoreAddedItems :: [a]
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
      ClientStore <$> o .: "added" <*> o .: "synced" <*> o .: "changed" <*> o .: "deleted"

instance (ToJSONKey i, ToJSON a) => ToJSON (ClientStore i a) where
  toJSON ClientStore {..} =
    object
      [ "added" .= clientStoreAddedItems
      , "synced" .= clientStoreSyncedItems
      , "changed" .= clientStoreSyncedButChangedItems
      , "deleted" .= clientStoreDeletedItems
      ]

emptyClientStore :: ClientStore i a
emptyClientStore =
  ClientStore
    { clientStoreAddedItems = []
    , clientStoreSyncedItems = M.empty
    , clientStoreSyncedButChangedItems = M.empty
    , clientStoreDeletedItems = M.empty
    }

newtype ServerStore i a =
  ServerStore
    { serverStoreItems :: Map i (Timed a)
    }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance (Validity i, Ord i, Validity a) => Validity (ServerStore i a)

emptyServerStore :: ServerStore i a
emptyServerStore = ServerStore {serverStoreItems = M.empty}

data SyncRequest i a =
  SyncRequest
    { syncRequestNewItems :: Map Int a -- Go back to a list here
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
      SyncRequest <$> o .: "new" <*> o .: "synced" <*> o .: "changed" <*> o .: "deleted"

instance (ToJSONKey i, ToJSON a) => ToJSON (SyncRequest i a) where
  toJSON SyncRequest {..} =
    object
      [ "new" .= syncRequestNewItems
      , "synced" .= syncRequestKnownItems
      , "changed" .= syncRequestKnownButChangedItems
      , "deleted" .= syncRequestDeletedItems
      ]

data SyncResponse i a =
  SyncResponse
    { syncResponseClientAdded :: Map Int (i, ServerTime) -- TODO replace by an intmap
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
      SyncResponse <$> o .: "client-added" <*> o .: "client-changed" <*> o .: "client-deleted" <*>
      o .: "server-added" <*>
      o .: "server-changed" <*>
      o .: "server-deleted" <*>
      o .: "conflict" <*>
      o .: "conflict-client-deleted" <*>
      o .: "conflict-server-deleted"

instance (ToJSON i, ToJSONKey i, ToJSON a) => ToJSON (SyncResponse i a) where
  toJSON SyncResponse {..} =
    object
      [ "client-added" .= syncResponseClientAdded
      , "client-changed" .= syncResponseClientChanged
      , "client-deleted" .= syncResponseClientDeleted
      , "server-added" .= syncResponseServerAdded
      , "server-changed" .= syncResponseServerChanged
      , "server-deleted" .= syncResponseServerDeleted
      , "conflict" .= syncResponseConflicts
      , "conflict-client-deleted" .= syncResponseConflictsClientDeleted
      , "conflict-server-deleted" .= syncResponseConflictsServerDeleted
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

makeSyncRequest :: ClientStore i a -> SyncRequest i a
makeSyncRequest ClientStore {..} =
  SyncRequest
    { syncRequestNewItems = addedItemsIntmap clientStoreAddedItems
    , syncRequestKnownItems = M.map timedTime clientStoreSyncedItems
    , syncRequestKnownButChangedItems = clientStoreSyncedButChangedItems
    , syncRequestDeletedItems = clientStoreDeletedItems
    }

mergeSyncResponseIgnoreProblems :: Ord i => ClientStore i a -> SyncResponse i a -> ClientStore i a
mergeSyncResponseIgnoreProblems cs SyncResponse {..} =
  let (addedItemsLeftovers, newSyncedItems) =
        mergeAddedItems (addedItemsIntmap (clientStoreAddedItems cs)) syncResponseClientAdded
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
      -- The M.difference calls only make sure that this function always produces valid values.
      -- They are not necessary for the correct working.
   in ClientStore
        { clientStoreAddedItems = addedItemsLeftovers
        , clientStoreSyncedButChangedItems = syncedButNotChangedLeftovers `M.difference` synced
        , clientStoreDeletedItems = deletedItemsLeftovers `M.difference` synced
        , clientStoreSyncedItems =
            synced `M.difference` (M.fromSet (const ()) syncResponseServerDeleted)
        }

addedItemsIntmap :: [a] -> Map Int a
addedItemsIntmap = M.fromList . zip [0 ..]

mergeAddedItems ::
     forall i a. Ord i
  => Map Int a
  -> Map Int (i, ServerTime)
  -> ([a], Map i (Timed a))
mergeAddedItems local added = M.foldlWithKey go ([], M.empty) local
  where
    go :: ([a], Map i (Timed a)) -> Int -> a -> ([a], Map i (Timed a))
    go (as, m) i a =
      case M.lookup i added of
        Nothing -> (a : as, m)
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
     Ord i => SyncResponse i a -> ClientId i -> ItemSyncResponse a -> SyncResponse i a
addToSyncResponse sr cid isr =
  case cid of
    BothServerAndClient i int ->
      case isr of
        ItemSyncResponseSuccesfullyAdded st ->
          sr {syncResponseClientAdded = M.insert int (i, st) $ syncResponseClientAdded sr}
        _ -> error "should not happen"
    OnlyServer i ->
      case isr of
        ItemSyncResponseInSyncEmpty -> sr
        ItemSyncResponseInSyncFull -> sr
        ItemSyncResponseSuccesfullyAdded _ -> error "should not happen."
        ItemSyncResponseSuccesfullyChanged st ->
          sr {syncResponseClientChanged = M.insert i st $ syncResponseClientChanged sr}
        ItemSyncResponseSuccesfullyDeleted ->
          sr {syncResponseClientDeleted = S.insert i $ syncResponseClientDeleted sr}
        ItemSyncResponseNewAtServer t ->
          sr {syncResponseServerAdded = M.insert i t $ syncResponseServerAdded sr}
        ItemSyncResponseModifiedAtServer t ->
          sr {syncResponseServerChanged = M.insert i t $ syncResponseServerChanged sr}
        ItemSyncResponseDeletedAtServer ->
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

-- | Process a sync request from the server
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
      unidentifedPairs :: Map Int (ServerItem a, ItemSyncRequest a)
      unidentifedPairs = M.map (\a -> (ServerEmpty, ItemSyncRequestNew a)) syncRequestNewItems
      unidentifedResults :: Map Int (ItemSyncResponse a, ServerItem a)
      unidentifedResults = M.map (uncurry processServerItemSync) unidentifedPairs
  generatedResults <-
    fmap M.fromList $
    forM (M.toList unidentifedResults) $ \(int, r) -> do
      uuid <- genId
      pure ((uuid, int), r)
  let identifiedResults :: Map i (ItemSyncResponse a, ServerItem a)
      identifiedResults = M.map (uncurry processServerItemSync) requestPairs
      allResults :: Map (ClientId i) (ItemSyncResponse a, ServerItem a)
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

data ClientId i
  = OnlyServer i
  | BothServerAndClient i Int
  deriving (Show, Eq, Ord, Generic)

distinct :: Eq a => [a] -> Bool
distinct ls = nub ls == ls

data Some a b
  = One a
  | Other b
  | Both a b
  deriving (Show, Eq, Generic)

unionThese :: Ord k => Map k a -> Map k b -> Map k (These a b)
unionThese m1 m2 = M.unionWith go (M.map This m1) (M.map That m2)
  where
    go (This a) (That b) = These a b
    go _ _ = error "should not happen."
