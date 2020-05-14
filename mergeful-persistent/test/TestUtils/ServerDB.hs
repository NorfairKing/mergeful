{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TestUtils.ServerDB where

import Control.Monad.State
import Data.GenValidity
import Data.GenValidity.Mergeful
import qualified Data.Map as M
import Data.Maybe
import Data.Mergeful
import Data.Mergeful.Persistent
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)

-- The thing that we'll synchronise on
newtype Thing = Thing {thingNumber :: Int}
  deriving (Show, Eq, Ord, Generic)

instance Validity Thing

instance GenUnchecked Thing

instance GenValid Thing

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

setupServerQuery :: ServerStore ServerThingId Thing -> SqlPersistT IO ()
setupServerQuery ServerStore {..} =
  forM_ (M.toList serverStoreItems) $ \(stid, tt) -> insertKey stid $ serverUnmakeThing tt

serverGetStoreQuery :: SqlPersistT IO (ServerStore ServerThingId Thing)
serverGetStoreQuery = ServerStore . M.fromList . map (\(Entity stid st) -> (stid, serverMakeThing st)) <$> selectList [] []

serverProcessSyncQuery :: Ord ci => SyncRequest ci ServerThingId Thing -> SqlPersistT IO (SyncResponse ci ServerThingId Thing)
serverProcessSyncQuery sreq = do
  -- FIXME this should be possible more efficiently.
  -- This should also be possible in a nicer way than juggling those ids
  store <- serverGetStoreQuery
  lastThingId <- fmap entityKey <$> selectFirst [] [Desc ServerThingId]
  let nextKey = toSqlKey . succ . fromSqlKey
      nextFreeId = maybe (toSqlKey 0) nextKey lastThingId
  let (resp, store') = evalState (processServerSync (state (\i -> (i, nextKey i))) store sreq) nextFreeId
  deleteWhere ([] :: [Filter ServerThing]) -- Clean slate
  setupServerQuery store'
  pure resp

serverMakeThing :: ServerThing -> Timed Thing
serverMakeThing ServerThing {..} = Timed {timedValue = Thing {thingNumber = serverThingNumber}, timedTime = serverThingTime}

serverUnmakeThing :: Timed Thing -> ServerThing
serverUnmakeThing Timed {..} = ServerThing {serverThingNumber = thingNumber timedValue, serverThingTime = timedTime}
