{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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

emptyClientStore :: ClientStore i a
emptyClientStore =
  ClientStore
    { clientStoreAddedItems = []
    , clientStoreSyncedItems = M.empty
    , clientStoreSyncedButChangedItems = M.empty
    , clientStoreDeletedItems = M.empty
    }

data ServerStore i a =
  ServerStore
    { serverStoreItems :: Map i (Timed a)
    }
  deriving (Show, Eq, Generic)

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

data SyncResponse i a =
  SyncResponse
    { syncResponseAddedItems :: Map Int (i, ServerTime) -- TODO replace by an intmap
    , syncResponseNewRemoteItems :: Map i (Timed a)
    , syncResponseModifiedByServerItems :: Map i (Timed a)
    , syncResponseModifiedByClientItems :: Map i ServerTime
    , syncResponseItemsToBeDeletedLocally :: Set i
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
        [ map (\(_, (i, _)) -> i) $ M.toList syncResponseAddedItems
        , M.keys syncResponseNewRemoteItems
        , M.keys syncResponseModifiedByServerItems
        , M.keys syncResponseModifiedByClientItems
        , S.toList syncResponseItemsToBeDeletedLocally
        , M.keys syncResponseConflicts
        , M.keys syncResponseConflictsClientDeleted
        , S.toList syncResponseConflictsServerDeleted
        ]
      ]

emptySyncResponse :: SyncResponse i a
emptySyncResponse =
  SyncResponse
    { syncResponseAddedItems = M.empty
    , syncResponseNewRemoteItems = M.empty
    , syncResponseModifiedByServerItems = M.empty
    , syncResponseModifiedByClientItems = M.empty
    , syncResponseItemsToBeDeletedLocally = S.empty
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
        mergeAddedItems (addedItemsIntmap (clientStoreAddedItems cs)) syncResponseAddedItems
      (syncedButNotChangedLeftovers, newModifiedItems) =
        mergeSyncedButChangedItems
          (clientStoreSyncedButChangedItems cs)
          syncResponseModifiedByClientItems
      deletedItemsLeftovers =
        mergeDeletedItems (clientStoreDeletedItems cs) syncResponseItemsToBeDeletedLocally
      synced =
        M.unions
          [ newSyncedItems
          , syncResponseNewRemoteItems
          , syncResponseModifiedByServerItems
          , newModifiedItems
          , clientStoreSyncedItems cs
          ]
      -- The M.difference calls only make sure that this function always produces valid values.
      -- They are not necessary for the correct working.
   in ClientStore
        { clientStoreAddedItems = addedItemsLeftovers
        , clientStoreSyncedButChangedItems = syncedButNotChangedLeftovers `M.difference` synced
        , clientStoreDeletedItems = deletedItemsLeftovers `M.difference` synced
        , clientStoreSyncedItems = synced
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
          sr {syncResponseAddedItems = M.insert int (i, st) $ syncResponseAddedItems sr}
        _ -> error "should not happen"
    OnlyServer i ->
      case isr of
        ItemSyncResponseInSyncEmpty -> sr
        ItemSyncResponseInSyncFull -> sr
        ItemSyncResponseSuccesfullyAdded _ -> error "should not happen."
        ItemSyncResponseSuccesfullyChanged st ->
          sr
            { syncResponseModifiedByClientItems =
                M.insert i st $ syncResponseModifiedByClientItems sr
            }
        ItemSyncResponseSuccesfullyDeleted ->
          sr
            { syncResponseItemsToBeDeletedLocally =
                S.insert i $ syncResponseItemsToBeDeletedLocally sr
            }
        ItemSyncResponseNewAtServer t ->
          sr {syncResponseNewRemoteItems = M.insert i t $ syncResponseNewRemoteItems sr}
        ItemSyncResponseModifiedAtServer t ->
          sr
            { syncResponseModifiedByServerItems =
                M.insert i t $ syncResponseModifiedByServerItems sr
            }
        ItemSyncResponseDeletedAtServer ->
          sr
            { syncResponseItemsToBeDeletedLocally =
                S.insert i $ syncResponseItemsToBeDeletedLocally sr
            }
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
      pairs :: Map i (ServerItem a, ItemSyncRequest a)
      pairs = M.map (fromThese ServerEmpty ItemSyncRequestPoll) thesePairs
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
      identifiedResults = M.map (uncurry processServerItemSync) pairs
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
