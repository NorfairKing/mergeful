{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Mergeful.Persistent
  ( -- * Server side
    serverProcessSyncQuery,

    -- ** Utils
    setupServerQuery,
    serverGetStoreQuery,
  )
where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Data.Mergeful
import Data.Set (Set)
import qualified Data.Set as S
import Database.Persist
import Database.Persist.Sql

deriving instance PersistField ServerTime

deriving instance PersistFieldSql ServerTime

serverProcessSyncQuery ::
  forall ci record a.
  ( PersistEntity record,
    PersistField (Key record),
    PersistEntityBackend record ~ SqlBackend,
    ToBackendKey SqlBackend record,
    Ord ci
  ) =>
  EntityField record (Key record) ->
  (Timed a -> record) ->
  (record -> Timed a) ->
  SyncRequest ci (Key record) a ->
  SqlPersistT IO (SyncResponse ci (Key record) a)
serverProcessSyncQuery idField makeFunc unmakeFunc sreq = do
  -- FIXME this should be possible more efficiently.
  -- This should also be possible in a nicer way than juggling those ids
  store <- serverGetStoreQuery unmakeFunc
  lastThingId <- fmap entityKey <$> selectFirst [] [Desc idField]
  -- This is a lot of nonsense just to make sure that we get fresh ids
  -- If we had a custom sync processor then we could just call 'insert' and be done with it.
  let roundSucc i = if i == maxBound then minBound else succ i
      nextKey :: Key record -> Key record
      nextKey = toSqlKey . roundSucc . fromSqlKey
      takenIds = M.keysSet $ serverStoreItems store
      nextFreeKey takens i =
        if i `S.member` takens
          then nextFreeKey takens (nextKey i) -- Keep looking
          else (i, S.insert i takens) -- this one is free, but will now be taken.
      firstFreeId :: Key record
      firstFreeId = fromMaybe (toSqlKey 0) lastThingId
      getNextFreeId = state $ \(i, takens) ->
        let (nf, takens') = nextFreeKey takens i
         in (nf, (nf, takens'))
  let (resp, store') =
        evalState
          ( processServerSync getNextFreeId store sreq ::
              State (Key record, Set (Key record))
                ( SyncResponse ci (Key record) a,
                  ServerStore (Key record) a
                )
          )
          (firstFreeId, takenIds)
  deleteWhere ([] :: [Filter record]) -- Clean slate
  setupServerQuery makeFunc store'
  pure resp

setupServerQuery ::
  forall record a.
  ( PersistEntity record,
    PersistField (Key record),
    PersistEntityBackend record ~ SqlBackend
  ) =>
  (Timed a -> record) ->
  ServerStore (Key record) a ->
  SqlPersistT IO ()
setupServerQuery func ServerStore {..} =
  forM_ (M.toList serverStoreItems) $ \(stid, tt) -> insertKey stid $ func tt

serverGetStoreQuery ::
  ( PersistEntity record,
    PersistField (Key record),
    PersistEntityBackend record ~ SqlBackend
  ) =>
  (record -> Timed a) ->
  SqlPersistT IO (ServerStore (Key record) a)
serverGetStoreQuery func = ServerStore . M.fromList . map (\(Entity stid st) -> (stid, func st)) <$> selectList [] []