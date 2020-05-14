{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TestUtils.ClientDB where

import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Mergeful
import Data.Mergeful.Persistent ()
import Data.Validity
import Data.Validity.Persist ()
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)
import TestUtils.ServerDB

share
  [mkPersist sqlSettings, mkMigrate "migrateClient"]
  [persistLowerCase|

ClientThing
  -- All the fields of 'Thing' go here.
  number Int
  serverId ServerThingId Maybe -- Nothing means it's not been synced
  serverTime ServerTime Maybe -- Nothing means it's not been synced
  deleted Bool -- True means this item has been tombstoned, and it must have been synced before.
  changed Bool -- True means it's been changed after it's been synced, it must have been synced before.

  ClientUniqueServerId serverId !force

  deriving Show
  deriving Eq
  deriving Ord
  deriving Generic

|]

instance Validity ClientThing where
  validate ct@ClientThing {..} =
    mconcat
      [ genericValidate ct,
        declare "The server id and time are either both Nothing or both Just" $ case (clientThingServerId, clientThingServerTime) of
          (Nothing, Nothing) -> True
          (Just _, Just _) -> True
          (Just _, Nothing) -> True
          (Nothing, Just _) -> True,
        declare "If it has been deleted, then it must have been synced" $
          if clientThingDeleted
            then isJust clientThingServerId
            else True,
        declare
          "If it has been changed, then it must have been synced before"
          $ if clientThingChanged
            then isJust clientThingServerId
            else True
      ]

setupUnsyncedClientQuery :: [Thing] -> SqlPersistT IO ()
setupUnsyncedClientQuery = mapM_ $ \Thing {..} ->
  insert_
    ClientThing
      { clientThingNumber = thingNumber,
        clientThingServerId = Nothing,
        clientThingServerTime = Nothing,
        clientThingDeleted = False,
        clientThingChanged = False
      }

setupClientQuery :: ClientStore ClientThingId ServerThingId Thing -> SqlPersistT IO ()
setupClientQuery ClientStore {..} = do
  forM_ (M.toList clientStoreAddedItems) $ \(cid, t) ->
    insertKey cid $ makeUnsyncedClientThing t
  forM_ (M.toList clientStoreSyncedItems) $ \(sid, tt) ->
    insert_ $ makeSyncedClientThing sid tt
  forM_ (M.toList clientStoreSyncedButChangedItems) $ \(sid, tt) ->
    insert_ $ makeSyncedButChangedClientThing sid tt
  forM_ (M.toList clientStoreDeletedItems) $ \(sid, st) -> insert_ $ makeDeletedClientThing sid st

clientGetStoreQuery :: SqlPersistT IO (ClientStore ClientThingId ServerThingId Thing)
clientGetStoreQuery = do
  clientStoreAddedItems <-
    M.fromList . map unmakeUnsyncedClientThing
      <$> selectList
        [ ClientThingServerId ==. Nothing,
          ClientThingServerTime ==. Nothing
        ]
        []
  clientStoreSyncedItems <-
    M.fromList . map unmakeSyncedClientThing
      <$> selectList
        [ ClientThingServerId !=. Nothing,
          ClientThingServerTime !=. Nothing,
          ClientThingChanged ==. False,
          ClientThingDeleted ==. False
        ]
        []
  clientStoreSyncedButChangedItems <-
    M.fromList . map unmakeSyncedClientThing
      <$> selectList
        [ ClientThingServerId !=. Nothing,
          ClientThingServerTime !=. Nothing,
          ClientThingChanged ==. True,
          ClientThingDeleted ==. False
        ]
        []
  clientStoreDeletedItems <-
    M.fromList . map unmakeDeletedClientThing
      <$> selectList
        [ ClientThingDeleted ==. True
        ]
        []
  pure ClientStore {..}

clientMakeSyncRequestQuery :: SqlPersistT IO (SyncRequest ClientThingId ServerThingId Thing)
clientMakeSyncRequestQuery = do
  syncRequestNewItems <-
    M.fromList . map unmakeUnsyncedClientThing
      <$> selectList
        [ ClientThingServerId ==. Nothing,
          ClientThingServerTime ==. Nothing
        ]
        []
  syncRequestKnownItems <-
    M.fromList . map unmakeDeletedClientThing
      <$> selectList
        [ ClientThingServerId !=. Nothing,
          ClientThingServerTime !=. Nothing,
          ClientThingChanged ==. False,
          ClientThingDeleted ==. False
        ]
        []
  syncRequestKnownButChangedItems <-
    M.fromList . map unmakeSyncedClientThing
      <$> selectList
        [ ClientThingServerId !=. Nothing,
          ClientThingServerTime !=. Nothing,
          ClientThingChanged ==. True,
          ClientThingDeleted ==. False
        ]
        []
  syncRequestDeletedItems <-
    M.fromList . map unmakeDeletedClientThing
      <$> selectList
        [ ClientThingDeleted ==. True
        ]
        []
  pure SyncRequest {..}

clientMergeSyncResponseQuery :: ItemMergeStrategy Thing -> SyncResponse ClientThingId ServerThingId Thing -> SqlPersistT IO ()
clientMergeSyncResponseQuery strat = mergeSyncResponseCustom strat clientSyncProcessor

clientSyncProcessor :: ClientSyncProcessor ClientThingId ServerThingId Thing (SqlPersistT IO)
clientSyncProcessor = ClientSyncProcessor {..}

unmakeUnsyncedClientThing :: Entity ClientThing -> (ClientThingId, Thing)
unmakeUnsyncedClientThing (Entity cid ClientThing {..}) = (cid, Thing {thingNumber = clientThingNumber})

makeUnsyncedClientThing :: Thing -> ClientThing
makeUnsyncedClientThing Thing {..} =
  ClientThing
    { clientThingNumber = thingNumber,
      clientThingDeleted = False,
      clientThingServerId = Nothing,
      clientThingChanged = False,
      clientThingServerTime = Nothing
    }

unmakeSyncedClientThing :: Entity ClientThing -> (ServerThingId, Timed Thing)
unmakeSyncedClientThing (Entity cid ClientThing {..}) =
  ( fromJust clientThingServerId,
    Timed
      { timedValue = Thing {thingNumber = clientThingNumber},
        timedTime = fromJust clientThingServerTime
      }
  )

makeSyncedClientThing :: ServerThingId -> Timed Thing -> ClientThing
makeSyncedClientThing sid (Timed Thing {..} st) =
  ClientThing
    { clientThingNumber = thingNumber,
      clientThingServerId = Just sid,
      clientThingServerTime = Just st,
      clientThingChanged = False,
      clientThingDeleted = False
    }

makeSyncedButChangedClientThing :: ServerThingId -> Timed Thing -> ClientThing
makeSyncedButChangedClientThing sid (Timed Thing {..} st) =
  ClientThing
    { clientThingNumber = thingNumber,
      clientThingServerId = Just sid,
      clientThingServerTime = Just st,
      clientThingChanged = False,
      clientThingDeleted = True
    }

unmakeDeletedClientThing :: Entity ClientThing -> (ServerThingId, ServerTime)
unmakeDeletedClientThing (Entity cid ClientThing {..}) =
  (fromJust clientThingServerId, fromJust clientThingServerTime)

makeDeletedClientThing :: ServerThingId -> ServerTime -> ClientThing
makeDeletedClientThing sid st =
  ClientThing
    { clientThingNumber = 0, -- dummy
      clientThingServerId = Just sid,
      clientThingServerTime = Just st,
      clientThingChanged = False, -- dummy
      clientThingDeleted = True
    }

clientMakeThing :: ClientThing -> Thing
clientMakeThing ClientThing {..} = Thing {thingNumber = clientThingNumber}
