{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TestUtils.ServerDB where

import Control.Monad.State
import Data.GenValidity
import Data.GenValidity.Mergeful ()
import qualified Data.Map as M
import Data.Maybe
import Data.Mergeful
import Data.Mergeful.Persistent ()
import Data.Set (Set)
import qualified Data.Set as S
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

serverProcessSyncQuery :: forall ci. Ord ci => SyncRequest ci ServerThingId Thing -> SqlPersistT IO (SyncResponse ci ServerThingId Thing)
serverProcessSyncQuery sreq = do
  -- FIXME this should be possible more efficiently.
  -- This should also be possible in a nicer way than juggling those ids
  store <- serverGetStoreQuery
  lastThingId <- fmap entityKey <$> selectFirst [] [Desc ServerThingId]
  -- This is a lot of nonsense just to make sure that we get fresh ids
  -- If we had a custom sync processor then we could just call 'insert' and be done with it.
  let roundSucc i = if i == maxBound then minBound else succ i
      nextKey = toSqlKey . roundSucc . fromSqlKey
      takenIds = M.keysSet $ serverStoreItems store
      nextFreeKey takens i =
        if i `S.member` takens
          then nextFreeKey takens (nextKey i) -- Keep looking
          else (i, S.insert i takens) -- this one is free, but will now be taken.
      firstFreeId = fromMaybe (toSqlKey 0) lastThingId
      getNextFreeId = state $ \(i, takens) ->
        let (nf, takens') = nextFreeKey takens i
         in (nf, (nf, takens'))
  let (resp, store') =
        evalState
          (processServerSync getNextFreeId store sreq :: State (ServerThingId, Set ServerThingId) (SyncResponse ci ServerThingId Thing, ServerStore ServerThingId Thing))
          (firstFreeId, takenIds)
  deleteWhere ([] :: [Filter ServerThing]) -- Clean slate
  setupServerQuery store'
  pure resp

serverMakeThing :: ServerThing -> Timed Thing
serverMakeThing ServerThing {..} = Timed {timedValue = Thing {thingNumber = serverThingNumber}, timedTime = serverThingTime}

serverUnmakeThing :: Timed Thing -> ServerThing
serverUnmakeThing Timed {..} = ServerThing {serverThingNumber = thingNumber timedValue, serverThingTime = timedTime}
