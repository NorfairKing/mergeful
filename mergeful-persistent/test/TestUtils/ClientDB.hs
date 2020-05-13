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

import Data.Mergeful
import Data.Mergeful.Persistent ()
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)
import TestUtils.ServerDB

share
  [mkPersist sqlSettings, mkMigrate "migrateClient"]
  [persistLowerCase|

ClientThing
  number Int
  serverId ServerThingId Maybe -- Nothing means it's not been synced
  deleted Bool -- True means this item has been tombstoned

  ClientUniqueServerId serverId !force

  deriving Show
  deriving Eq
  deriving Ord
  deriving Generic

|]

setupUnsyncedClientQuery :: [ServerThing] -> SqlPersistT IO ()
setupUnsyncedClientQuery sts = undefined sts

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
      clientThingServerId = Nothing
    }

makeSyncedClientThing :: ServerThingId -> ServerThing -> ClientThing
makeSyncedClientThing sid ServerThing {..} =
  ClientThing
    { clientThingNumber = serverThingNumber,
      clientThingDeleted = False,
      clientThingServerId = Just sid
    }

makeDeletedClientThing :: ServerThingId -> ClientThing
makeDeletedClientThing sid =
  ClientThing
    { clientThingNumber = 0, -- dummy
      clientThingDeleted = True,
      clientThingServerId = Just sid
    }

makeServerThing :: ClientThing -> ServerThing
makeServerThing ClientThing {..} = ServerThing {serverThingNumber = clientThingNumber}
