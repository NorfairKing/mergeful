{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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
module Data.Mergeful.Collection
  ( initialClientStore,
    clientStoreSize,
    clientStoreClientIdSet,
    clientStoreUndeletedSyncIdSet,
    clientStoreSyncIdSet,
    clientStoreItems,
    addItemToClientStore,
    findFreeSpot,
    markItemDeletedInClientStore,
    changeItemInClientStore,
    deleteItemFromClientStore,
    initialSyncRequest,
    makeSyncRequest,
    mergeSyncResponseIgnoreProblems,
    mergeSyncResponseFromServer,

    -- * Custom merging
    ItemMergeStrategy (..),
    mergeSyncResponseUsingStrategy,

    -- * Server side
    initialServerStore,
    processServerSync,

    -- * Types, for reference
    ClientStore (..),
    SyncRequest (..),
    ClientAddition (..),
    SyncResponse (..),
    emptySyncResponse,
    ServerStore (..),
    ClientId (..),

    -- * Utility functions for implementing client-side merging
    mergeAddedItems,
    mergeSyncedButChangedItems,
    mergeDeletedItems,

    -- * Utility functions for implementing server-side responding
    addToSyncResponse,
  )
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Mergeful.Item
import Data.Mergeful.Timed
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Validity
import Data.Validity.Containers ()
import Data.Word
import GHC.Generics (Generic)

-- | A Client-side identifier for items.
--
-- These only need to be unique at the client.
newtype ClientId
  = ClientId
      { unClientId :: Word64
      }
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

instance Validity ClientId

instance NFData ClientId

data ClientStore ci si a
  = ClientStore
      { -- | These items are new locally but have not been synced to the server yet.
        clientStoreAddedItems :: Map ci a,
        -- | These items have been synced at their respective 'ServerTime's.
        clientStoreSyncedItems :: Map si (Timed a),
        -- | These items have been synced at their respective 'ServerTime's
        -- but modified locally since then.
        clientStoreSyncedButChangedItems :: Map si (Timed a),
        -- | These items have been deleted locally after they were synced
        -- but the server has not been notified of that yet.
        clientStoreDeletedItems :: Map si ServerTime
      }
  deriving (Show, Eq, Generic)

instance
  (Validity ci, Validity si, Show ci, Show si, Ord ci, Ord si, Validity a) =>
  Validity (ClientStore ci si a)
  where
  validate cs@ClientStore {..} =
    mconcat
      [ genericValidate cs,
        declare "There are no duplicate IDs"
          $ distinct
          $ concat
            [ M.keys clientStoreSyncedItems,
              M.keys clientStoreSyncedButChangedItems,
              M.keys clientStoreDeletedItems
            ]
      ]

instance (NFData ci, NFData si, NFData a) => NFData (ClientStore ci si a)

instance
  (Ord ci, Ord si, FromJSONKey ci, FromJSONKey si, FromJSON a) =>
  FromJSON (ClientStore ci si a)
  where
  parseJSON =
    withObject "ClientStore" $ \o ->
      ClientStore <$> o .:? "added" .!= M.empty <*> o .:? "synced" .!= M.empty
        <*> o .:? "changed" .!= M.empty
        <*> o .:? "deleted" .!= M.empty

instance (ToJSONKey ci, ToJSONKey si, ToJSON a) => ToJSON (ClientStore ci si a) where
  toJSON ClientStore {..} =
    object $
      catMaybes
        [ jNull "added" clientStoreAddedItems,
          jNull "synced" clientStoreSyncedItems,
          jNull "changed" clientStoreSyncedButChangedItems,
          jNull "deleted" clientStoreDeletedItems
        ]

-- | A client store to start with.
--
-- This store contains no items.
initialClientStore :: ClientStore ci si a
initialClientStore =
  ClientStore
    { clientStoreAddedItems = M.empty,
      clientStoreSyncedItems = M.empty,
      clientStoreSyncedButChangedItems = M.empty,
      clientStoreDeletedItems = M.empty
    }

-- | The number of items in a client store
--
-- This does not count the deleted items, so that they really look deleted..
clientStoreSize :: ClientStore ci si a -> Word
clientStoreSize ClientStore {..} =
  fromIntegral $
    sum
      [ M.size clientStoreAddedItems,
        M.size clientStoreSyncedItems,
        M.size clientStoreSyncedButChangedItems
      ]

-- | The set of client ids.
--
-- These are only the client ids of the added items that have not been synced yet.
clientStoreClientIdSet :: ClientStore ci si a -> Set ci
clientStoreClientIdSet ClientStore {..} = M.keysSet clientStoreAddedItems

-- | The set of server ids.
--
-- This does not include the ids of items that have been marked as deleted.
clientStoreUndeletedSyncIdSet :: Ord si => ClientStore ci si a -> Set si
clientStoreUndeletedSyncIdSet ClientStore {..} =
  S.unions [M.keysSet clientStoreSyncedItems, M.keysSet clientStoreSyncedButChangedItems]

-- | The set of server ids.
--
-- This includes the ids of items that have been marked as deleted.
clientStoreSyncIdSet :: (Ord ci, Ord si) => ClientStore ci si a -> Set si
clientStoreSyncIdSet ClientStore {..} =
  S.unions
    [ M.keysSet clientStoreSyncedItems,
      M.keysSet clientStoreSyncedButChangedItems,
      M.keysSet clientStoreDeletedItems
    ]

-- | The set of items in the client store
--
-- This map does not include items that have been marked as deleted.
clientStoreItems :: (Ord ci, Ord si) => ClientStore ci si a -> Map (Either ci si) a
clientStoreItems ClientStore {..} =
  M.unions
    [ M.mapKeys Left clientStoreAddedItems,
      M.mapKeys Right $ M.map timedValue clientStoreSyncedItems,
      M.mapKeys Right $ M.map timedValue clientStoreSyncedButChangedItems
    ]

-- | Add an item to a client store as an added item.
--
-- This will take care of the uniqueness constraint of the 'ci's in the map.
addItemToClientStore ::
  (Ord ci, Enum ci, Bounded ci) => a -> ClientStore ci si a -> ClientStore ci si a
addItemToClientStore a cs =
  let oldAddedItems = clientStoreAddedItems cs
      newAddedItems =
        let newKey = findFreeSpot oldAddedItems
         in M.insert newKey a oldAddedItems
   in cs {clientStoreAddedItems = newAddedItems}

-- | Find a free client id to use
--
-- You shouldn't need this function, 'addItemToClientStore' takes care of this.
--
-- The values wrap around when reaching 'maxBound'.
findFreeSpot :: (Ord ci, Enum ci, Bounded ci) => Map ci a -> ci
findFreeSpot m =
  if M.null m
    then minBound
    else
      let (i, _) = M.findMax m
       in go (next i)
  where
    go i =
      if M.member i m
        then go (next i)
        else i
    next ci
      | ci == maxBound = minBound
      | otherwise = succ ci

-- | Mark an item deleted in a client store.
--
-- This function will not delete the item, but mark it as deleted instead.
markItemDeletedInClientStore :: Ord si => si -> ClientStore ci si a -> ClientStore ci si a
markItemDeletedInClientStore u cs =
  let oldSyncedItems = clientStoreSyncedItems cs
      oldChangedItems = clientStoreSyncedButChangedItems cs
      oldDeletedItems = clientStoreDeletedItems cs
      mItem = M.lookup u oldSyncedItems <|> M.lookup u oldChangedItems
   in case mItem of
        Nothing -> cs
        Just t ->
          let newSyncedItems = M.delete u oldSyncedItems
              newChangedItems = M.delete u oldChangedItems
              newDeletedItems = M.insert u (timedTime t) oldDeletedItems
           in cs
                { clientStoreSyncedItems = newSyncedItems,
                  clientStoreSyncedButChangedItems = newChangedItems,
                  clientStoreDeletedItems = newDeletedItems
                }

-- | Replace the given item with a new value.
--
-- This function will correctly mark the item as changed, if it exist.
--
-- It will not add an item to the store with the given id, because the
-- server may not have been the origin of that id.
changeItemInClientStore :: Ord si => si -> a -> ClientStore ci si a -> ClientStore ci si a
changeItemInClientStore i a cs =
  case M.lookup i (clientStoreSyncedItems cs) of
    Just t ->
      cs
        { clientStoreSyncedItems = M.delete i (clientStoreSyncedItems cs),
          clientStoreSyncedButChangedItems =
            M.insert i (t {timedValue = a}) (clientStoreSyncedButChangedItems cs)
        }
    Nothing ->
      case M.lookup i (clientStoreSyncedButChangedItems cs) of
        Nothing -> cs
        Just _ ->
          cs
            { clientStoreSyncedButChangedItems =
                M.adjust (\t -> t {timedValue = a}) i (clientStoreSyncedButChangedItems cs)
            }

-- | Delete an unsynced item from a client store.
--
-- This function will immediately delete the item, because it has never been synced.
deleteItemFromClientStore :: Ord ci => ci -> ClientStore ci si a -> ClientStore ci si a
deleteItemFromClientStore i cs = cs {clientStoreAddedItems = M.delete i (clientStoreAddedItems cs)}

newtype ServerStore si a
  = ServerStore
      { -- | A map of items, named using an 'i', together with the 'ServerTime' at which
        -- they were last synced.
        serverStoreItems :: Map si (Timed a)
      }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance (Validity si, Show si, Ord si, Validity a) => Validity (ServerStore si a)

instance (NFData si, NFData a) => NFData (ServerStore si a)

-- | A server store to start with
--
-- This store contains no items.
initialServerStore :: ServerStore si a
initialServerStore = ServerStore {serverStoreItems = M.empty}

data SyncRequest ci si a
  = SyncRequest
      { -- | These items are new locally but have not been synced to the server yet.
        syncRequestNewItems :: Map ci a,
        -- | These items have been synced at their respective 'ServerTime's.
        syncRequestKnownItems :: Map si ServerTime,
        -- | These items have been synced at their respective 'ServerTime's
        -- but modified locally since then.
        syncRequestKnownButChangedItems :: Map si (Timed a),
        -- | These items have been deleted locally after they were synced
        -- but the server has not been notified of that yet.
        syncRequestDeletedItems :: Map si ServerTime
      }
  deriving (Show, Eq, Generic)

instance
  (Validity ci, Validity si, Show ci, Show si, Ord ci, Ord si, Validity a) =>
  Validity (SyncRequest ci si a)
  where
  validate sr@SyncRequest {..} =
    mconcat
      [ genericValidate sr,
        declare "There are no duplicate IDs"
          $ distinct
          $ concat
            [ M.keys syncRequestKnownItems,
              M.keys syncRequestKnownButChangedItems,
              M.keys syncRequestDeletedItems
            ]
      ]

instance (NFData ci, NFData si, NFData a) => NFData (SyncRequest ci si a)

instance
  (Ord ci, Ord si, FromJSONKey ci, FromJSONKey si, FromJSON a) =>
  FromJSON (SyncRequest ci si a)
  where
  parseJSON =
    withObject "SyncRequest" $ \o ->
      SyncRequest <$> o .:? "new" .!= M.empty <*> o .:? "synced" .!= M.empty
        <*> o .:? "changed" .!= M.empty
        <*> o .:? "deleted" .!= M.empty

instance (ToJSONKey ci, ToJSONKey si, ToJSON a) => ToJSON (SyncRequest ci si a) where
  toJSON SyncRequest {..} =
    object $
      catMaybes
        [ jNull "new" syncRequestNewItems,
          jNull "synced" syncRequestKnownItems,
          jNull "changed" syncRequestKnownButChangedItems,
          jNull "deleted" syncRequestDeletedItems
        ]

-- | An intial 'SyncRequest' to start with.
--
-- It just asks the server to send over whatever it knows.
initialSyncRequest :: SyncRequest ci si a
initialSyncRequest =
  SyncRequest
    { syncRequestNewItems = M.empty,
      syncRequestKnownItems = M.empty,
      syncRequestKnownButChangedItems = M.empty,
      syncRequestDeletedItems = M.empty
    }

data ClientAddition i
  = ClientAddition
      { clientAdditionId :: i,
        clientAdditionServerTime :: ServerTime
      }
  deriving (Show, Eq, Generic)

instance Validity i => Validity (ClientAddition i)

instance NFData i => NFData (ClientAddition i)

instance FromJSON i => FromJSON (ClientAddition i) where
  parseJSON = withObject "ClientAddition" $ \o -> ClientAddition <$> o .: "id" <*> o .: "time"

instance ToJSON i => ToJSON (ClientAddition i) where
  toJSON ClientAddition {..} = object ["id" .= clientAdditionId, "time" .= clientAdditionServerTime]

data SyncResponse ci si a
  = SyncResponse
      { -- | The client added these items and server has succesfully been made aware of that.
        --
        -- The client needs to update their server times
        syncResponseClientAdded :: Map ci (ClientAddition si),
        -- | The client changed these items and server has succesfully been made aware of that.
        --
        -- The client needs to update their server times
        syncResponseClientChanged :: Map si ServerTime,
        -- | The client deleted these items and server has succesfully been made aware of that.
        --
        -- The client can delete them from its deleted items
        syncResponseClientDeleted :: Set si,
        -- | These items have been added on the server side
        --
        -- The client should add them too.
        syncResponseServerAdded :: Map si (Timed a),
        -- | These items have been modified on the server side.
        --
        -- The client should modify them too.
        syncResponseServerChanged :: Map si (Timed a),
        -- | These items were deleted on the server side
        --
        -- The client should delete them too
        syncResponseServerDeleted :: Set si,
        -- | These are conflicts where the server and the client both have an item, but it is different.
        --
        -- The server kept its part of each, the client can either take whatever the server gave them
        -- or deal with the conflicts somehow, and then try to re-sync.
        syncResponseConflicts :: Map si (Timed a),
        -- | These are conflicts where the server has an item but the client does not.
        --
        -- The server kept its item, the client can either take whatever the server gave them
        -- or deal with the conflicts somehow, and then try to re-sync.
        syncResponseConflictsClientDeleted :: Map si (Timed a),
        -- | These are conflicts where the server has no item but the client has a modified item.
        --
        -- The server left its item deleted, the client can either delete its items too
        -- or deal with the conflicts somehow, and then try to re-sync.
        syncResponseConflictsServerDeleted :: Set si
      }
  deriving (Show, Eq, Generic)

instance
  (Validity ci, Validity si, Show ci, Show si, Ord ci, Ord si, Validity a) =>
  Validity (SyncResponse ci si a)
  where
  validate sr@SyncResponse {..} =
    mconcat
      [ genericValidate sr,
        declare "There are no duplicate IDs"
          $ distinct
          $ concat
            [ map (\(_, ClientAddition {..}) -> clientAdditionId) $ M.toList syncResponseClientAdded,
              M.keys syncResponseClientChanged,
              S.toList syncResponseClientDeleted,
              M.keys syncResponseServerAdded,
              M.keys syncResponseServerChanged,
              S.toList syncResponseServerDeleted,
              M.keys syncResponseConflicts,
              M.keys syncResponseConflictsClientDeleted,
              S.toList syncResponseConflictsServerDeleted
            ]
      ]

instance (NFData ci, NFData si, NFData a) => NFData (SyncResponse ci si a)

instance
  (Ord ci, Ord si, FromJSON ci, FromJSON si, FromJSONKey ci, FromJSONKey si, FromJSON a) =>
  FromJSON (SyncResponse ci si a)
  where
  parseJSON =
    withObject "SyncResponse" $ \o ->
      SyncResponse <$> o .:? "client-added" .!= M.empty <*> o .:? "client-changed" .!= M.empty
        <*> o .:? "client-deleted" .!= S.empty
        <*> o .:? "server-added" .!= M.empty
        <*> o .:? "server-changed" .!= M.empty
        <*> o .:? "server-deleted" .!= S.empty
        <*> o .:? "conflict" .!= M.empty
        <*> o .:? "conflict-client-deleted" .!= M.empty
        <*> o .:? "conflict-server-deleted" .!= S.empty

instance
  (ToJSON ci, ToJSON si, ToJSONKey ci, ToJSONKey si, ToJSON a) =>
  ToJSON (SyncResponse ci si a)
  where
  toJSON SyncResponse {..} =
    object $
      catMaybes
        [ jNull "client-added" syncResponseClientAdded,
          jNull "client-changed" syncResponseClientChanged,
          jNull "client-deleted" syncResponseClientDeleted,
          jNull "server-added" syncResponseServerAdded,
          jNull "server-changed" syncResponseServerChanged,
          jNull "server-deleted" syncResponseServerDeleted,
          jNull "conflict" syncResponseConflicts,
          jNull "conflict-client-deleted" syncResponseConflictsClientDeleted,
          jNull "conflict-server-deleted" syncResponseConflictsServerDeleted
        ]

emptySyncResponse :: SyncResponse ci si a
emptySyncResponse =
  SyncResponse
    { syncResponseClientAdded = M.empty,
      syncResponseClientChanged = M.empty,
      syncResponseClientDeleted = S.empty,
      syncResponseServerAdded = M.empty,
      syncResponseServerChanged = M.empty,
      syncResponseServerDeleted = S.empty,
      syncResponseConflicts = M.empty,
      syncResponseConflictsClientDeleted = M.empty,
      syncResponseConflictsServerDeleted = S.empty
    }

jNull :: (Foldable f, ToJSON (f b)) => Text -> f b -> Maybe (Text, Value)
jNull n s =
  if null s
    then Nothing
    else Just $ n .= s

-- | Produce an 'SyncRequest' from a 'ClientStore'.
--
-- Send this to the server for synchronisation.
makeSyncRequest :: ClientStore ci si a -> SyncRequest ci si a
makeSyncRequest ClientStore {..} =
  SyncRequest
    { syncRequestNewItems = clientStoreAddedItems,
      syncRequestKnownItems = M.map timedTime clientStoreSyncedItems,
      syncRequestKnownButChangedItems = clientStoreSyncedButChangedItems,
      syncRequestDeletedItems = clientStoreDeletedItems
    }

-- | Merge an 'SyncResponse' into the current 'ClientStore'.
--
-- This function ignores any problems that may occur.
-- In the case of a conclict, it will just not update the client item.
-- The next sync request will then produce a conflict again.
--
-- Pro: No data will be lost
--
-- __Con: Clients will diverge when conflicts occur.__
mergeSyncResponseIgnoreProblems ::
  (Ord ci, Ord si) => ClientStore ci si a -> SyncResponse ci si a -> ClientStore ci si a
mergeSyncResponseIgnoreProblems cs SyncResponse {..} =
  let (addedItemsLeftovers, newSyncedItems) =
        mergeAddedItems (clientStoreAddedItems cs) syncResponseClientAdded
      (syncedButNotChangedLeftovers, newModifiedItems) =
        mergeSyncedButChangedItems (clientStoreSyncedButChangedItems cs) syncResponseClientChanged
      deletedItemsLeftovers =
        mergeDeletedItems (clientStoreDeletedItems cs) syncResponseClientDeleted
      synced =
        M.unions
          [ newSyncedItems,
            syncResponseServerAdded,
            syncResponseServerChanged,
            newModifiedItems,
            clientStoreSyncedItems cs
          ]
   in ClientStore
        { clientStoreAddedItems = addedItemsLeftovers,
          clientStoreSyncedButChangedItems = syncedButNotChangedLeftovers `M.difference` synced,
          clientStoreDeletedItems = deletedItemsLeftovers `M.difference` synced,
          clientStoreSyncedItems =
            synced `M.difference` M.fromSet (const ()) syncResponseServerDeleted
        }

-- | Merge an 'SyncResponse' into the current 'ClientStore' with the given merge strategy.
--
-- In order for clients to converge on the same collection correctly, this function must be:
--
-- * Associative
-- * Idempotent
-- * The same on all clients
--
-- This function ignores mismatches.
mergeSyncResponseUsingStrategy ::
  (Ord ci, Ord si) =>
  ItemMergeStrategy a ->
  ClientStore ci si a ->
  SyncResponse ci si a ->
  ClientStore ci si a
mergeSyncResponseUsingStrategy ItemMergeStrategy {..} cs SyncResponse {..} =
  let (addedItemsLeftovers, newSyncedItems) =
        mergeAddedItems (clientStoreAddedItems cs) syncResponseClientAdded
      (syncedButNotChangedLeftovers, newModifiedItems) =
        mergeSyncedButChangedItems (clientStoreSyncedButChangedItems cs) syncResponseClientChanged
      deletedItemsLeftovers =
        mergeDeletedItems (clientStoreDeletedItems cs) syncResponseClientDeleted
      synced =
        M.unions
          [ newSyncedItems,
            syncResponseServerAdded,
            syncResponseServerChanged,
            -- Merge the synced but changed (the only ones that could have caused a conflict)
            -- with the ones that the response indicated were a conflict.
            M.intersectionWith
              (\ci (Timed si st) -> Timed (itemMergeStrategyMergeChangeConflict ci si) st)
              (M.map timedValue $ clientStoreSyncedButChangedItems cs)
              syncResponseConflicts,
            -- Of the items that the server changed but the client deleted,
            -- keep the ones that the strategy wants to keep.
            M.mapMaybe id $
              M.intersectionWith
                ( \_ (Timed si st) ->
                    Timed <$> itemMergeStrategyMergeClientDeletedConflict si <*> pure st
                )
                (clientStoreDeletedItems cs)
                syncResponseConflictsClientDeleted,
            newModifiedItems,
            clientStoreSyncedItems cs
          ]
      newSyncedButChangedItems =
        syncedButNotChangedLeftovers
          `M.difference` M.fromSet (const ()) syncResponseConflictsServerDeleted
   in ClientStore
        { clientStoreAddedItems = addedItemsLeftovers,
          clientStoreSyncedButChangedItems = newSyncedButChangedItems `M.difference` synced,
          clientStoreDeletedItems = deletedItemsLeftovers `M.difference` synced,
          clientStoreSyncedItems =
            synced `M.difference` M.fromSet (const ()) syncResponseServerDeleted
        }

data ClientSyncProcessor ci si a (m :: * -> *)
  = ClientSyncProcessor
      { clientSyncProcessorSyncClientAdded :: Map ci (ClientAddition si) -> m (),
        clientSyncProcessorSyncClientChanged :: Map si ServerTime -> m (),
        clientSyncProcessorSyncClientDeleted :: Set si -> m (),
        clientSyncProcessorSyncServerAdded :: Map si (Timed a) -> m (),
        clientSyncProcessorSyncServerChanged :: Map si (Timed a) -> m (),
        clientSyncProcessorSyncServerDeleted :: Set si -> m (),
        clientSyncProcessorSyncChangeConflict :: Map si (Timed a) -> m (),
        clientSyncProcessorSyncConflictsClientDeleted :: Map si (Timed a) -> m (),
        clientSyncProcessorSyncConflictsServerDeleted :: Set si -> m ()
      }
  deriving (Generic)

mergeSyncResponseCustom :: Monad m => ClientSyncProcessor ci si a m -> SyncResponse ci si a -> m ()
mergeSyncResponseCustom ClientSyncProcessor {..} SyncResponse {..} = do
  -- The order here matters!
  clientSyncProcessorSyncChangeConflict syncResponseConflicts
  clientSyncProcessorSyncConflictsClientDeleted syncResponseConflictsClientDeleted
  clientSyncProcessorSyncConflictsServerDeleted syncResponseConflictsServerDeleted
  clientSyncProcessorSyncServerAdded syncResponseServerAdded
  clientSyncProcessorSyncServerChanged syncResponseServerChanged
  clientSyncProcessorSyncServerDeleted syncResponseServerDeleted
  clientSyncProcessorSyncClientDeleted syncResponseClientDeleted
  clientSyncProcessorSyncClientChanged syncResponseClientChanged
  clientSyncProcessorSyncClientAdded syncResponseClientAdded

-- | Merge an 'SyncResponse' into the current 'ClientStore' by taking whatever the server gave the client.
--
-- Pro: Clients will converge on the same value.
--
-- __Con: Conflicting updates will be lost.__
mergeSyncResponseFromServer ::
  (Ord ci, Ord si) => ClientStore ci si a -> SyncResponse ci si a -> ClientStore ci si a
mergeSyncResponseFromServer =
  mergeSyncResponseUsingStrategy
    ItemMergeStrategy
      { itemMergeStrategyMergeChangeConflict = \_ serverItem -> serverItem,
        itemMergeStrategyMergeClientDeletedConflict = \serverItem -> Just serverItem,
        itemMergeStrategyMergeServerDeletedConflict = \_ -> Nothing
      }

-- | Merge the local added items with the ones that the server has acknowledged as added.
mergeAddedItems ::
  forall ci si a.
  (Ord ci, Ord si) =>
  Map ci a ->
  Map ci (ClientAddition si) ->
  (Map ci a, Map si (Timed a))
mergeAddedItems local added = M.foldlWithKey go (M.empty, M.empty) local
  where
    go :: (Map ci a, Map si (Timed a)) -> ci -> a -> (Map ci a, Map si (Timed a))
    go (as, m) ci a =
      case M.lookup ci added of
        Nothing -> (M.insert ci a as, m)
        Just ClientAddition {..} ->
          ( as,
            M.insert
              clientAdditionId
              (Timed {timedValue = a, timedTime = clientAdditionServerTime})
              m
          )

-- | Merge the local synced but changed items with the ones that the server has acknowledged as changed.
mergeSyncedButChangedItems ::
  forall i a.
  Ord i =>
  Map i (Timed a) ->
  Map i ServerTime ->
  (Map i (Timed a), Map i (Timed a))
mergeSyncedButChangedItems local changed = M.foldlWithKey go (M.empty, M.empty) local
  where
    go :: (Map i (Timed a), Map i (Timed a)) -> i -> Timed a -> (Map i (Timed a), Map i (Timed a))
    go (m1, m2) k t =
      case M.lookup k changed of
        Nothing -> (M.insert k t m1, m2)
        Just st' -> (m1, M.insert k (t {timedTime = st'}) m2)

-- | Merge the local deleted items with the ones that the server has acknowledged as deleted.
mergeDeletedItems :: Ord i => Map i b -> Set i -> Map i b
mergeDeletedItems m s = m `M.difference` M.fromSet (const ()) s

data Identifier ci si
  = OnlyServer si
  | BothServerAndClient si ci
  deriving (Show, Eq, Ord, Generic)

instance (Validity ci, Validity si) => Validity (Identifier ci si)

instance (NFData ci, NFData si) => NFData (Identifier ci si)

-- | Serve an 'SyncRequest' using the current 'ServerStore', producing an 'SyncResponse' and a new 'ServerStore'.
processServerSync ::
  forall ci si a m.
  ( Ord ci,
    Ord si,
    Monad m
  ) =>
  -- | The action that is guaranteed to generate unique identifiers
  m si ->
  ServerStore si a ->
  SyncRequest ci si a ->
  m (SyncResponse ci si a, ServerStore si a)
processServerSync genId ServerStore {..} sr@SyncRequest {..} =
  -- Make tuples of requests for all of the items that only had a client identifier.
  do
    let unidentifedPairs :: Map ci (ServerItem a, ItemSyncRequest a)
        unidentifedPairs = M.map (\a -> (ServerEmpty, ItemSyncRequestNew a)) syncRequestNewItems
        -- Make tuples of results for each of the unidentifier tuples.
        unidentifedResults :: Map ci (ItemSyncResponse a, ServerItem a)
        unidentifedResults = M.map (uncurry processServerItemSync) unidentifedPairs
    generatedResults <- generateIdentifiersFor genId unidentifedResults
    -- Gather the items that had a server identifier already.
    let clientIdentifiedSyncRequests :: Map si (ItemSyncRequest a)
        clientIdentifiedSyncRequests = identifiedItemSyncRequests sr
        -- Make 'ServerItem's for each of the items on the server side
        serverIdentifiedItems :: Map si (ServerItem a)
        serverIdentifiedItems = M.map ServerFull serverStoreItems
        -- Match up client items with server items by their id.
        thesePairs :: Map si (These (ServerItem a) (ItemSyncRequest a))
        thesePairs = unionTheseMaps serverIdentifiedItems clientIdentifiedSyncRequests
        -- Make tuples of server 'ServerItem's and 'ItemSyncRequest's for each of the items with an id
        requestPairs :: Map si (ServerItem a, ItemSyncRequest a)
        requestPairs = M.map (fromThese ServerEmpty ItemSyncRequestPoll) thesePairs
        -- Make tuples of results for each of the tuplus that had a server identifier.
        identifiedResults :: Map si (ItemSyncResponse a, ServerItem a)
        identifiedResults = M.map (uncurry processServerItemSync) requestPairs
    -- Put together the results together
    let allResults :: Map (Identifier ci si) (ItemSyncResponse a, ServerItem a)
        allResults =
          M.union
            (M.mapKeys OnlyServer identifiedResults)
            (M.mapKeys (uncurry BothServerAndClient) generatedResults)
    pure $ produceSyncResults allResults

identifiedItemSyncRequests :: (Ord ci, Ord si) => SyncRequest ci si a -> Map si (ItemSyncRequest a)
identifiedItemSyncRequests SyncRequest {..} =
  M.unions
    [ M.map ItemSyncRequestKnown syncRequestKnownItems,
      M.map ItemSyncRequestKnownButChanged syncRequestKnownButChangedItems,
      M.map ItemSyncRequestDeletedLocally syncRequestDeletedItems
    ]

generateIdentifiersFor ::
  (Ord ci, Ord si, Monad m) =>
  m si ->
  Map ci (ItemSyncResponse a, ServerItem a) ->
  m (Map (si, ci) (ItemSyncResponse a, ServerItem a))
generateIdentifiersFor genId unidentifedResults =
  fmap M.fromList
    $ forM (M.toList unidentifedResults)
    $ \(int, r) -> do
      uuid <- genId
      pure ((uuid, int), r)

produceSyncResults ::
  forall ci si a.
  (Ord ci, Ord si) =>
  Map (Identifier ci si) (ItemSyncResponse a, ServerItem a) ->
  (SyncResponse ci si a, ServerStore si a)
produceSyncResults allResults =
  -- Produce a sync response
  let resp :: SyncResponse ci si a
      resp =
        M.foldlWithKey
          (\sr cid (isr, _) -> addToSyncResponse sr cid isr)
          emptySyncResponse
          allResults
      -- Produce a new server store
      newStore :: Map si (Timed a)
      newStore =
        M.mapMaybe
          ( \case
              ServerEmpty -> Nothing
              ServerFull t -> Just t
          )
          $ M.map snd
          $ M.mapKeys
            ( \case
                OnlyServer i -> i
                BothServerAndClient i _ -> i
            )
            allResults
   in -- return them both.
      (resp, ServerStore newStore)

-- | Given an incomplete 'SyncResponse', an id, possibly a client ID too, and
-- an 'ItemSyncResponse', produce a less incomplete 'SyncResponse'.
addToSyncResponse ::
  (Ord ci, Ord si) =>
  SyncResponse ci si a ->
  Identifier ci si ->
  ItemSyncResponse a ->
  SyncResponse ci si a
addToSyncResponse sr cid isr =
  case cid of
    BothServerAndClient i int ->
      case isr of
        ItemSyncResponseClientAdded st ->
          sr
            { syncResponseClientAdded =
                M.insert int (ClientAddition i st) $ syncResponseClientAdded sr
            }
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

unionTheseMaps :: Ord k => Map k a -> Map k b -> Map k (These a b)
unionTheseMaps m1 m2 = M.unionWith go (M.map This m1) (M.map That m2)
  where
    go (This a) (That b) = These a b
    go _ _ = error "should not happen."

distinct :: Eq a => [a] -> Bool
distinct ls = nub ls == ls

-- Inlined because holy smokes, `these` has a _lot_ of dependencies.
data These a b
  = This a
  | That b
  | These a b
  deriving (Show, Eq, Generic)

fromThese :: a -> b -> These a b -> (a, b)
fromThese a b t =
  case t of
    This a' -> (a', b)
    That b' -> (a, b')
    These a' b' -> (a', b')
