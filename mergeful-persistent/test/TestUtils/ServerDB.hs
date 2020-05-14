{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TestUtils.ServerDB where

import Data.GenValidity
import Data.GenValidity.Mergeful
import Data.Mergeful
import Data.Mergeful.Persistent
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)

-- The thing that we'll synchronise on
newtype Thing = Thing {thingNumber :: Int}

share
  [mkPersist sqlSettings, mkMigrate "migrateServer"]
  [persistLowerCase|

ServerThing
  -- All the fields of 'Thing' go here.
  number Int
  time ServerTime

  deriving Show
  deriving Eq
  deriving Ord
  deriving Generic

|]

instance Validity ServerThing

instance GenUnchecked ServerThing

instance GenValid ServerThing

setupServerQuery :: ServerStore ServerThingId ServerThing -> SqlPersistT IO ()
setupServerQuery ServerStore {..} = pure ()

serverGetStoreQuery :: SqlPersistT IO (ServerStore ServerThingId ServerThing)
serverGetStoreQuery = pure undefined

serverProcessSyncQuery :: SyncRequest ci ServerThingId ServerThing -> SqlPersistT IO (SyncResponse ci ServerThingId ServerThing)
serverProcessSyncQuery SyncRequest {..} = pure undefined
