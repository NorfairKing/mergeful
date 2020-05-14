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

setupUnsyncedClientQuery :: [ServerThing] -> SqlPersistT IO ()
setupUnsyncedClientQuery = undefined

setupClientQuery :: ClientStore ClientThingId ServerThingId ServerThing -> SqlPersistT IO ()
setupClientQuery ClientStore {..} = pure ()

clientGetStoreQuery :: SqlPersistT IO (ClientStore ClientThingId ServerThingId ServerThing)
clientGetStoreQuery = pure undefined

clientMakeSyncRequestQuery :: SqlPersistT IO (SyncRequest ClientThingId ServerThingId ServerThing)
clientMakeSyncRequestQuery = pure undefined

clientMergeSyncResponseQuery :: SyncResponse ClientThingId ServerThingId ServerThing -> SqlPersistT IO ()
clientMergeSyncResponseQuery SyncResponse {..} = pure ()

makeUnsyncedClientThing :: ServerThing -> ClientThing
makeUnsyncedClientThing ServerThing {..} =
  ClientThing
    { clientThingNumber = serverThingNumber,
      clientThingDeleted = False,
      clientThingServerId = Nothing,
      clientThingChanged = False,
      clientThingServerTime = Nothing
    }

makeSyncedClientThing :: ServerThingId -> Timed ServerThing -> ClientThing
makeSyncedClientThing sid (Timed ServerThing {..} st) =
  ClientThing
    { clientThingNumber = serverThingNumber,
      clientThingServerId = Just sid,
      clientThingServerTime = Just st,
      clientThingChanged = False,
      clientThingDeleted = False
    }

makeDeletedClientThing :: ServerThingId -> ClientThing
makeDeletedClientThing sid =
  ClientThing
    { clientThingNumber = 0, -- dummy
      clientThingServerId = Just sid,
      clientThingServerTime = Just initialServerTime, -- dummy
      clientThingChanged = False, -- dummy
      clientThingDeleted = True
    }

makeThing :: ClientThing -> Thing
makeThing ClientThing {..} = Thing {thingNumber = clientThingNumber}
