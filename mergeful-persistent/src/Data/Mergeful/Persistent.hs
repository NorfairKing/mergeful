{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Mergeful.Persistent
  ( -- * Client side
    clientMakeSyncRequestQuery,
    clientMergeSyncResponseQuery,
    clientSyncProcessor,

    -- ** Merging
    ItemMergeStrategy (..),
    mergeFromServerStrategy,
    mergeFromClientStrategy,
    mergeUsingCRDTStrategy,

    -- * Server side
    serverProcessSyncQuery,
    serverProcessSyncWithCustomIdQuery,

    -- * Utils

    -- ** Client side
    setupClientQuery,
    clientGetStoreQuery,

    -- ** Server side
    setupServerQuery,
    serverGetStoreQuery,
  )
where

import Control.Monad
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Mergeful
import Data.Set (Set)
import qualified Data.Set as S
import Database.Persist
import Database.Persist.Sql

deriving instance PersistField ServerTime

deriving instance PersistFieldSql ServerTime

-- | Make a sync request
clientMakeSyncRequestQuery ::
  forall record sid a.
  ( Ord sid,
    PersistEntity record,
    PersistField (Key record),
    PersistField sid,
    PersistEntityBackend record ~ SqlBackend,
    ToBackendKey SqlBackend record
  ) =>
  -- | The server id field
  EntityField record (Maybe sid) ->
  -- | The server time field
  EntityField record (Maybe ServerTime) ->
  -- | The changed flag
  EntityField record Bool ->
  -- | The deleted flag
  EntityField record Bool ->
  -- | How to read an unsynced client item
  (Entity record -> (Key record, a)) ->
  -- | How to read a synced client item
  (Entity record -> (sid, Timed a)) ->
  -- | How to read a deleted client item
  (Entity record -> (sid, ServerTime)) ->
  SqlPersistT IO (SyncRequest (Key record) sid a)
clientMakeSyncRequestQuery
  serverIdField
  serverTimeField
  changedField
  deletedField
  unmakeUnsyncedClientThing
  unmakeSyncedClientThing
  unmakeDeletedClientThing = do
    syncRequestNewItems <-
      M.fromList . map unmakeUnsyncedClientThing
        <$> selectList
          [ serverIdField ==. Nothing,
            serverTimeField ==. Nothing
          ]
          []
    syncRequestKnownItems <-
      M.fromList . map unmakeDeletedClientThing
        <$> selectList
          [ serverIdField !=. Nothing,
            serverTimeField !=. Nothing,
            changedField ==. False,
            deletedField ==. False
          ]
          []
    syncRequestKnownButChangedItems <-
      M.fromList . map unmakeSyncedClientThing
        <$> selectList
          [ serverIdField !=. Nothing,
            serverTimeField !=. Nothing,
            changedField ==. True,
            deletedField ==. False
          ]
          []
    syncRequestDeletedItems <-
      M.fromList . map unmakeDeletedClientThing
        <$> selectList
          [ deletedField ==. True
          ]
          []
    pure SyncRequest {..}

clientMergeSyncResponseQuery ::
  forall record sid a.
  ( Ord sid,
    PersistEntity record,
    PersistField (Key record),
    PersistField sid,
    PersistEntityBackend record ~ SqlBackend,
    ToBackendKey SqlBackend record
  ) =>
  -- | The server id field
  EntityField record (Maybe sid) ->
  -- | The server time field
  EntityField record (Maybe ServerTime) ->
  -- | The changed flag
  EntityField record Bool ->
  -- | The deleted flag
  EntityField record Bool ->
  -- | How to build a synced record from a server id and a timed item
  (sid -> Timed a -> record) ->
  -- | How to read a synced record back into a server id and a timed item
  (Entity record -> (sid, Timed a)) ->
  -- | How to update a row with new data
  --
  -- You only need to perform the updates that have anything to do with the data to sync.
  -- The housekeeping updates are already taken care of.
  (a -> [Update record]) ->
  -- | The merge strategy.
  ItemMergeStrategy a ->
  -- | The sync response to merge
  SyncResponse (Key record) sid a ->
  SqlPersistT IO ()
clientMergeSyncResponseQuery
  serverIdField
  serverTimeField
  changedField
  deletedField
  makeSyncedClientThing
  unmakeSyncedClientThing
  recordUpdates
  strat =
    mergeSyncResponseCustom strat $
      clientSyncProcessor
        serverIdField
        serverTimeField
        changedField
        deletedField
        makeSyncedClientThing
        unmakeSyncedClientThing
        recordUpdates

clientSyncProcessor ::
  forall record sid a.
  ( Ord sid,
    PersistEntity record,
    PersistField (Key record),
    PersistField sid,
    PersistEntityBackend record ~ SqlBackend,
    ToBackendKey SqlBackend record
  ) =>
  -- | The server id field
  EntityField record (Maybe sid) ->
  -- | The server time field
  EntityField record (Maybe ServerTime) ->
  -- | The changed flag
  EntityField record Bool ->
  -- | The deleted flag
  EntityField record Bool ->
  -- | How to build a synced record from a server id and a timed item
  (sid -> Timed a -> record) ->
  -- | How to read a synced record back into a server id and a timed item
  (Entity record -> (sid, Timed a)) ->
  -- | How to update a row with new data
  --
  -- You only need to perform the updates that have anything to do with the data to sync.
  -- The housekeeping updates are already taken care of.
  (a -> [Update record]) ->
  ClientSyncProcessor (Key record) sid a (SqlPersistT IO)
clientSyncProcessor
  serverIdField
  serverTimeField
  changedField
  deletedField
  makeSyncedClientThing
  unmakeSyncedClientThing
  recordUpdates = ClientSyncProcessor {..}
    where
      clientSyncProcessorQuerySyncedButChangedValues :: Set sid -> SqlPersistT IO (Map sid (Timed a))
      clientSyncProcessorQuerySyncedButChangedValues si = fmap (M.fromList . map unmakeSyncedClientThing . catMaybes) $ forM (S.toList si) $ \sid ->
        selectFirst
          [ serverIdField ==. Just sid,
            serverTimeField !=. Nothing,
            changedField ==. True,
            deletedField ==. False
          ]
          []
      clientSyncProcessorSyncClientAdded :: Map (Key record) (ClientAddition sid) -> SqlPersistT IO ()
      clientSyncProcessorSyncClientAdded m = forM_ (M.toList m) $ \(cid, ClientAddition {..}) ->
        update
          cid
          [ serverIdField =. Just clientAdditionId,
            serverTimeField =. Just clientAdditionServerTime
          ]
      clientSyncProcessorSyncClientChanged :: Map sid ServerTime -> SqlPersistT IO ()
      clientSyncProcessorSyncClientChanged m = forM_ (M.toList m) $ \(sid, st) ->
        updateWhere
          [serverIdField ==. Just sid]
          [ serverTimeField =. Just st,
            changedField =. False
          ]
      clientSyncProcessorSyncClientDeleted :: Set sid -> SqlPersistT IO ()
      clientSyncProcessorSyncClientDeleted s = forM_ (S.toList s) $ \sid ->
        deleteWhere [serverIdField ==. Just sid]
      clientSyncProcessorSyncMergedConflict :: Map sid (Timed a) -> SqlPersistT IO ()
      clientSyncProcessorSyncMergedConflict m = forM_ (M.toList m) $ \(sid, Timed a st) ->
        updateWhere
          [serverIdField ==. Just sid]
          $ [ serverTimeField =. Just st,
              changedField =. True
            ]
            ++ recordUpdates a
      clientSyncProcessorSyncServerAdded :: Map sid (Timed a) -> SqlPersistT IO ()
      clientSyncProcessorSyncServerAdded m =
        insertMany_ $ map (uncurry makeSyncedClientThing) (M.toList m)
      clientSyncProcessorSyncServerChanged :: Map sid (Timed a) -> SqlPersistT IO ()
      clientSyncProcessorSyncServerChanged m = forM_ (M.toList m) $ \(sid, Timed a st) -> do
        updateWhere
          [serverIdField ==. Just sid]
          $ [ serverTimeField =. Just st,
              changedField =. False
            ]
            ++ recordUpdates a
      clientSyncProcessorSyncServerDeleted :: Set sid -> SqlPersistT IO ()
      clientSyncProcessorSyncServerDeleted s = forM_ (S.toList s) $ \sid ->
        deleteWhere [serverIdField ==. Just sid]

-- | Set up a client store.
--
-- You shouldn't need this.
setupClientQuery ::
  forall record sid a.
  ( PersistEntity record,
    PersistField (Key record),
    PersistEntityBackend record ~ SqlBackend,
    ToBackendKey SqlBackend record
  ) =>
  (a -> record) ->
  (sid -> Timed a -> record) ->
  (sid -> Timed a -> record) ->
  (sid -> ServerTime -> record) ->
  ClientStore (Key record) sid a ->
  SqlPersistT IO ()
setupClientQuery makeUnsyncedClientThing makeSyncedClientThing makeSyncedButChangedClientThing makeDeletedClientThing ClientStore {..} = do
  forM_ (M.toList clientStoreAddedItems) $ \(cid, t) ->
    insertKey cid $ makeUnsyncedClientThing t
  forM_ (M.toList clientStoreSyncedItems) $ \(sid, tt) ->
    insert_ $ makeSyncedClientThing sid tt
  forM_ (M.toList clientStoreSyncedButChangedItems) $ \(sid, tt) ->
    insert_ $ makeSyncedButChangedClientThing sid tt
  forM_ (M.toList clientStoreDeletedItems) $ \(sid, st) -> insert_ $ makeDeletedClientThing sid st

-- | Get the client store.
--
-- You shouldn't need this.
clientGetStoreQuery ::
  forall record sid a.
  ( Ord sid,
    PersistEntity record,
    PersistField (Key record),
    PersistField sid,
    PersistEntityBackend record ~ SqlBackend,
    ToBackendKey SqlBackend record
  ) =>
  EntityField record (Maybe sid) ->
  EntityField record (Maybe ServerTime) ->
  EntityField record Bool ->
  EntityField record Bool ->
  (Entity record -> (Key record, a)) ->
  (Entity record -> (sid, Timed a)) ->
  (Entity record -> (sid, ServerTime)) ->
  SqlPersistT IO (ClientStore (Key record) sid a)
clientGetStoreQuery serverIdField serverTimeField changedField deletedField unmakeUnsyncedClientThing unmakeSyncedClientThing unmakeDeletedClientThing = do
  clientStoreAddedItems <-
    M.fromList . map unmakeUnsyncedClientThing
      <$> selectList
        [ serverIdField ==. Nothing,
          serverTimeField ==. Nothing
        ]
        []
  clientStoreSyncedItems <-
    M.fromList . map unmakeSyncedClientThing
      <$> selectList
        [ serverIdField !=. Nothing,
          serverTimeField !=. Nothing,
          changedField ==. False,
          deletedField ==. False
        ]
        []
  clientStoreSyncedButChangedItems <-
    M.fromList . map unmakeSyncedClientThing
      <$> selectList
        [ serverIdField !=. Nothing,
          serverTimeField !=. Nothing,
          changedField ==. True,
          deletedField ==. False
        ]
        []
  clientStoreDeletedItems <-
    M.fromList . map unmakeDeletedClientThing
      <$> selectList
        [ deletedField ==. True
        ]
        []
  pure ClientStore {..}

-- | Process a sync request on the server side
serverProcessSyncQuery ::
  forall cid record a.
  ( PersistEntity record,
    PersistField (Key record),
    PersistEntityBackend record ~ SqlBackend,
    ToBackendKey SqlBackend record,
    Ord cid
  ) =>
  -- | The server time field
  EntityField record ServerTime ->
  -- | How to load an item from the database
  (Entity record -> (Key record, Timed a)) ->
  -- | How to add an item in the database with initial server time
  (a -> record) ->
  -- | How to update a record given new data
  (a -> [Update record]) ->
  -- | A sync request
  SyncRequest cid (Key record) a ->
  SqlPersistT IO (SyncResponse cid (Key record) a)
serverProcessSyncQuery serverTimeField unmakeFunc makeFunc recordUpdates sr = do
  let serverSyncProcessorRead = M.fromList . map unmakeFunc <$> selectList [] []
      serverSyncProcessorAddItem = insert . makeFunc
      serverSyncProcessorChangeItem si st a = update si $ (serverTimeField =. st) : recordUpdates a
      serverSyncProcessorDeleteItem = delete
      proc = ServerSyncProcessor {..} :: ServerSyncProcessor cid (Key record) a (SqlPersistT IO)
  processServerSyncCustom proc sr

-- | Process a sync request on the server side with a custom id field
--
-- You can use this function if you want to use a UUID as your id instead of the sqlkey of the item.
serverProcessSyncWithCustomIdQuery ::
  forall cid sid record a.
  ( PersistEntity record,
    PersistField sid,
    PersistEntityBackend record ~ SqlBackend,
    ToBackendKey SqlBackend record,
    Ord cid,
    Ord sid
  ) =>
  -- | The custom id field
  EntityField record sid ->
  -- | The generator to generate the custom id field
  SqlPersistT IO sid ->
  -- | The server time field
  EntityField record ServerTime ->
  -- | How to load an item from the database
  (Entity record -> (sid, Timed a)) ->
  -- | How to add an item in the database with initial server time
  (sid -> a -> record) ->
  -- | How to update a record given new data
  (a -> [Update record]) ->
  -- | A sync request
  SyncRequest cid sid a ->
  SqlPersistT IO (SyncResponse cid sid a)
serverProcessSyncWithCustomIdQuery idField uuidGen serverTimeField unmakeFunc makeFunc recordUpdates sr = do
  let serverSyncProcessorRead = M.fromList . map unmakeFunc <$> selectList [] []
      serverSyncProcessorAddItem a = do
        uuid <- uuidGen
        insert_ $ makeFunc uuid a
        pure uuid
      serverSyncProcessorChangeItem si st a = updateWhere [idField ==. si] $ (serverTimeField =. st) : recordUpdates a
      serverSyncProcessorDeleteItem si = deleteWhere [idField ==. si]
      proc = ServerSyncProcessor {..} :: ServerSyncProcessor cid sid a (SqlPersistT IO)
  processServerSyncCustom proc sr

-- | Set up the server store
--
-- You shouldn't need this.
setupServerQuery ::
  forall sid record a.
  ( PersistEntity record,
    PersistField (Key record),
    PersistEntityBackend record ~ SqlBackend
  ) =>
  (sid -> Timed a -> Entity record) ->
  ServerStore sid a ->
  SqlPersistT IO ()
setupServerQuery func ServerStore {..} =
  forM_ (M.toList serverStoreItems) $ \(sid, tt) ->
    let (Entity k r) = func sid tt
     in insertKey k r

-- | Get the server store
--
-- You shouldn't need this.
serverGetStoreQuery ::
  ( Ord sid,
    PersistEntity record,
    PersistField sid,
    PersistEntityBackend record ~ SqlBackend
  ) =>
  (Entity record -> (sid, Timed a)) ->
  SqlPersistT IO (ServerStore sid a)
serverGetStoreQuery func = ServerStore . M.fromList . map func <$> selectList [] []
