{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Mergeful.Persistent.TwoClientsSpec
  ( spec,
  )
where

import Control.Monad.Reader
import Data.List
import qualified Data.Map as M
import Data.Mergeful
import qualified Data.Set as S
import Database.Persist.Sql
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity
import TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = modifyMaxShrinks (const 0) $ twoClientsSpec $ do
  describe "sanity" $ do
    describe "setupClient & clientGetStore" $ do
      it "roundtrips" $ \te -> forAllValid $ \cstore -> runTest te $ do
        setupClient A cstore
        cstore' <- clientGetStore A
        liftIO $ cstore' `shouldBe` cstore
    describe "setupServer & serverGetStore" $ do
      it "roundtrips" $ \te -> forAllValid $ \sstore -> runTest te $ do
        setupServer sstore
        sstore' <- serverGetStore
        liftIO $ sstore' `shouldBe` sstore
  describe "mergeFromServerStrategy" $ mergeFunctionSpec mergeFromServerStrategy
  describe "mergeFromClientStrategy" $ mergeFunctionSpec mergeFromClientStrategy
  describe "mergeUsingCRDTStrategy" $ mergeFunctionSpec $ mergeUsingCRDTStrategy max

mergeFunctionSpec :: ItemMergeStrategy Thing -> SpecWith TestEnv
mergeFunctionSpec strat = do
  let mergeFunc = clientMergeSyncResponse strat
      syncFunc = sync strat
  describe "Multiple clients" $ do
    describe "Single item" $ do
      it "successfully syncs an addition accross to a second client" $ \te -> forAllValid $ \k -> forAllValid $ \i -> runTest te $ do
        -- Client A has one item
        setupClient A $ initialClientStore {clientStoreAddedItems = M.singleton k i}
        -- Client B is empty
        setupClient B initialClientStore
        -- The server is empty
        setupServer initialServerStore
        -- Client A makes sync request 1
        req1 <- clientMakeSyncRequest A
        -- The server processes sync request 1
        resp1 <- serverProcessSync req1
        sstore2 <- serverGetStore
        let addedItems = syncResponseClientAdded resp1
        case M.toList addedItems of
          [(k', ClientAddition uuid st)] -> do
            lift $ k' `shouldBe` k
            let time = initialServerTime
            lift $ st `shouldBe` time
            let items = M.singleton uuid (Timed i st)
            lift $ sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
            -- Client A merges the response
            mergeFunc A resp1
            cAstore2 <- clientGetStore A
            lift $ cAstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
            -- Client B makes sync request 2
            req2 <- clientMakeSyncRequest B
            cBstore2 <- clientGetStore B
            -- The server processes sync request 2
            resp2 <- serverProcessSync req2
            sstore3 <- serverGetStore
            lift $ do
              resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
              sstore3 `shouldBe` sstore2
            -- Client B merges the response
            mergeFunc B resp2
            cBstore2 <- clientGetStore B
            lift $ cBstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
            -- Client A and Client B now have the same store
            lift $ cAstore2 `shouldBe` cBstore2
          _ ->
            lift $
              expectationFailure
                "Should have found exactly one added item."
      it "successfully syncs a modification accross to a second client" $ \te -> forAllValid $ \uuid -> forAllValid $ \i -> forAllValid $ \j -> forAllValid $ \time1 ->
        runTest te $ do
          -- Client A has a synced item.
          setupClient A $
            initialClientStore
              { clientStoreSyncedItems = M.singleton uuid (Timed i time1)
              }
          -- Client B had synced that same item, but has since modified it
          setupClient B $
            initialClientStore
              { clientStoreSyncedButChangedItems = M.singleton uuid (Timed j time1)
              }
          -- The server is has the item that both clients had before
          setupServer $ ServerStore {serverStoreItems = M.singleton uuid (Timed i time1)}
          -- Client B makes sync request 1
          req1 <- clientMakeSyncRequest B
          -- The server processes sync request 1
          resp1 <- serverProcessSync req1
          sstore2 <- serverGetStore
          let time2 = incrementServerTime time1
          lift $ do
            resp1
              `shouldBe` emptySyncResponse {syncResponseClientChanged = M.singleton uuid time2}
            sstore2
              `shouldBe` ServerStore {serverStoreItems = M.singleton uuid (Timed j time2)}
          -- Client B merges the response
          mergeFunc B resp1
          cBstore2 <- clientGetStore B
          lift $
            cBstore2
              `shouldBe` initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed j time2)}
          -- Client A makes sync request 2
          req2 <- clientMakeSyncRequest A
          -- The server processes sync request 2
          resp2 <- serverProcessSync req2
          sstore3 <- serverGetStore
          lift $ do
            resp2
              `shouldBe` emptySyncResponse
                { syncResponseServerChanged = M.singleton uuid (Timed j time2)
                }
            sstore3 `shouldBe` sstore2
          -- Client A merges the response
          mergeFunc A resp2
          cAstore2 <- clientGetStore A
          lift $
            cAstore2
              `shouldBe` initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed j time2)}
          -- Client A and Client B now have the same store
          lift $ cAstore2 `shouldBe` cBstore2
      it "succesfully syncs a deletion across to a second client" $ \te -> forAllValid $ \uuid -> forAllValid $ \time1 -> forAllValid $ \i ->
        runTest te $ do
          setupClient A $
            initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed i time1)}
          -- Client A has a synced item.
          -- Client B had synced that same item, but has since deleted it.
          setupClient B $ initialClientStore {clientStoreDeletedItems = M.singleton uuid time1}
          -- The server still has the undeleted item
          setupServer $ ServerStore {serverStoreItems = M.singleton uuid (Timed i time1)}
          -- Client B makes sync request 1
          req1 <- clientMakeSyncRequest B
          -- The server processes sync request 1
          resp1 <- serverProcessSync req1
          sstore2 <- serverGetStore
          lift $ do
            resp1 `shouldBe` emptySyncResponse {syncResponseClientDeleted = S.singleton uuid}
            sstore2 `shouldBe` initialServerStore
          -- Client B merges the response
          mergeFunc B resp1
          cBstore2 <- clientGetStore B
          lift $ cBstore2 `shouldBe` initialClientStore
          -- Client A makes sync request 2
          req2 <- clientMakeSyncRequest A
          -- The server processes sync request 2
          resp2 <- serverProcessSync req2
          sstore3 <- serverGetStore
          lift $ do
            resp2 `shouldBe` emptySyncResponse {syncResponseServerDeleted = S.singleton uuid}
            sstore3 `shouldBe` sstore2
          -- Client A merges the response
          mergeFunc A resp2
          cAstore2 <- clientGetStore A
          lift $ cAstore2 `shouldBe` initialClientStore
          -- Client A and Client B now have the same store
          lift $ cAstore2 `shouldBe` cBstore2
      it "does not run into a conflict if two clients both try to sync a deletion" $ \te -> forAllValid $ \uuid -> forAllValid $ \time1 -> forAllValid $ \i ->
        runTest te $ do
          setupClient A $ initialClientStore {clientStoreDeletedItems = M.singleton uuid time1}
          -- Both client a and client b delete an item.
          setupClient B $ initialClientStore {clientStoreDeletedItems = M.singleton uuid time1}
          -- The server still has the undeleted item
          setupServer $ ServerStore {serverStoreItems = M.singleton uuid (Timed i time1)}
          -- Client A makes sync request 1
          req1 <- clientMakeSyncRequest A
          -- The server processes sync request 1
          resp1 <- serverProcessSync req1
          sstore2 <- serverGetStore
          lift $ do
            resp1
              `shouldBe` (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
            sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty})
          -- Client A merges the response
          mergeFunc A resp1
          cAstore2 <- clientGetStore A
          lift $ cAstore2 `shouldBe` initialClientStore
          -- Client B makes sync request 2
          req2 <- clientMakeSyncRequest B
          -- The server processes sync request 2
          resp2 <- serverProcessSync req2
          sstore3 <- serverGetStore
          lift $ do
            resp2
              `shouldBe` (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
            sstore3 `shouldBe` sstore2
          -- Client B merges the response
          mergeFunc B resp2
          cBstore2 <- clientGetStore B
          lift $ do
            cBstore2 `shouldBe` initialClientStore
            -- Client A and Client B now have the same store
            cAstore2 `shouldBe` cBstore2
    describe "Multiple items" $ do
      pure ()

type T a = ReaderT TestEnv IO a

runTest :: TestEnv -> T a -> IO a
runTest = flip runReaderT

runClientDB :: Client -> SqlPersistT IO a -> T a
runClientDB num func = do
  pool <- asks $ case num of
    A -> testEnvClient1Pool
    B -> testEnvClient2Pool
  liftIO $ runSqlPool func pool

runServerDB :: SqlPersistT IO a -> T a
runServerDB func = do
  pool <- asks testEnvServerPool
  liftIO $ runSqlPool func pool

type CS = ClientStore ClientThingId ServerThingId Thing

type SReq = SyncRequest ClientThingId ServerThingId Thing

type SS = ServerStore ServerThingId Thing

type SResp = SyncResponse ClientThingId ServerThingId Thing

sync :: ItemMergeStrategy Thing -> Client -> T (CS, SS, SS, CS)
sync strat client = do
  cstore1 <- clientGetStore client
  req <- clientMakeSyncRequest client
  sstore1 <- serverGetStore
  resp <- serverProcessSync req
  sstore2 <- serverGetStore
  clientMergeSyncResponse strat client resp
  cstore2 <- clientGetStore client
  pure (cstore1, sstore1, sstore2, cstore2)

-- setupUnsyncedClient :: [Thing] -> T ()
-- setupUnsyncedClient = runClientDB . setupUnsyncedClientQuery

setupClient :: Client -> CS -> T ()
setupClient client = runClientDB client . setupClientQuery

setupServer :: SS -> T ()
setupServer = runServerDB . setupServerQuery

clientGetStore :: Client -> T CS
clientGetStore client = runClientDB client clientGetStoreQuery

clientMakeSyncRequest :: Client -> T SReq
clientMakeSyncRequest client = runClientDB client clientMakeSyncRequestQuery

serverGetStore :: T SS
serverGetStore = runServerDB serverGetStoreQuery

serverProcessSync :: SReq -> T SResp
serverProcessSync = runServerDB . serverProcessSyncQuery

clientMergeSyncResponse :: ItemMergeStrategy Thing -> Client -> SResp -> T ()
clientMergeSyncResponse strat client = runClientDB client . clientMergeSyncResponseQuery strat

data Client = A | B
  deriving (Show, Eq)

data TestEnv
  = TestEnv
      { testEnvServerPool :: ConnectionPool,
        testEnvClient1Pool :: ConnectionPool,
        testEnvClient2Pool :: ConnectionPool
      }

twoClientsSpec :: SpecWith TestEnv -> Spec
twoClientsSpec = around withTestEnv

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv func =
  withServerPool $ \serverPool ->
    withClientPool 1 $ \client1Pool ->
      withClientPool 2 $ \client2Pool -> do
        let tenv =
              TestEnv
                { testEnvServerPool = serverPool,
                  testEnvClient1Pool = client1Pool,
                  testEnvClient2Pool = client2Pool
                }
        liftIO $ func tenv
