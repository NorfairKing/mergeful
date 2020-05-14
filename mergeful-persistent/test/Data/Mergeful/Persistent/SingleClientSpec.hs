{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Mergeful.Persistent.SingleClientSpec
  ( spec,
  )
where

import Control.Monad
import Control.Monad.Reader
import Data.GenValidity.Mergeful
import Data.List
import qualified Data.Map as M
import Data.Mergeful
import Data.Mergeful.Persistent
import Database.Persist.Sql
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = modifyMaxShrinks (const 0) $ oneClientSpec $ do
  describe "mergeFromServerStrategy" $ mergeFunctionSpec mergeFromServerStrategy
  describe "mergeFromClientStrategy" $ mergeFunctionSpec mergeFromClientStrategy
  describe "mergeUsingCRDTStrategy" $ mergeFunctionSpec $ mergeUsingCRDTStrategy max

mergeFunctionSpec :: ItemMergeStrategy Thing -> SpecWith TestEnv
mergeFunctionSpec strat = do
  let mergeFunc = clientMergeSyncResponse strat
  describe "Single Client" $ do
    describe "Single item" $ do
      it "succesfully downloads an item from the server for an empty client" $ \te ->
        forAllValid $ \sstore1 -> runTest te $ do
          setupClient initialClientStore
          setupServer sstore1
          req <- clientMakeSyncRequest
          resp <- serverProcessSync req
          sstore2 <- serverGetStore
          mergeFunc resp
          cstore2 <- clientGetStore
          lift $ do
            sstore2 `shouldBe` sstore1
            clientStoreSyncedItems cstore2 `shouldBe` serverStoreItems sstore2

type T a = ReaderT TestEnv IO a

runTest :: TestEnv -> T a -> IO a
runTest = flip runReaderT

runClientDB :: SqlPersistT IO a -> T a
runClientDB func = do
  pool <- asks testEnvClientPool
  liftIO $ runSqlPool func pool

runServerDB :: SqlPersistT IO a -> T a
runServerDB func = do
  pool <- asks testEnvServerPool
  liftIO $ runSqlPool func pool

type CS = ClientStore ClientThingId ServerThingId Thing

type SReq = SyncRequest ClientThingId ServerThingId Thing

type SS = ServerStore ServerThingId Thing

type SResp = SyncResponse ClientThingId ServerThingId Thing

sync :: ItemMergeStrategy Thing -> T (CS, SS, SS, CS)
sync strat = do
  cstore1 <- clientGetStore
  req <- clientMakeSyncRequest
  sstore1 <- serverGetStore
  resp <- serverProcessSync req
  sstore2 <- serverGetStore
  clientMergeSyncResponse strat resp
  cstore2 <- clientGetStore
  pure (cstore1, sstore1, sstore2, cstore2)

setupUnsyncedClient :: [Thing] -> T ()
setupUnsyncedClient = runClientDB . setupUnsyncedClientQuery

setupClient :: CS -> T ()
setupClient = runClientDB . setupClientQuery

setupServer :: SS -> T ()
setupServer = runServerDB . setupServerQuery

clientGetStore :: T CS
clientGetStore = runClientDB clientGetStoreQuery

clientMakeSyncRequest :: T SReq
clientMakeSyncRequest = runClientDB clientMakeSyncRequestQuery

serverGetStore :: T SS
serverGetStore = runServerDB serverGetStoreQuery

serverProcessSync :: SReq -> T SResp
serverProcessSync = runServerDB . serverProcessSyncQuery

clientMergeSyncResponse :: ItemMergeStrategy Thing -> SResp -> T ()
clientMergeSyncResponse strat = runClientDB . clientMergeSyncResponseQuery strat

data TestEnv
  = TestEnv
      { testEnvClientPool :: ConnectionPool,
        testEnvServerPool :: ConnectionPool
      }

oneClientSpec :: SpecWith TestEnv -> Spec
oneClientSpec = around withTestEnv

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv func =
  withServerPool $ \serverPool -> withClientPool 1 $ \clientPool -> do
    let tenv = TestEnv {testEnvClientPool = clientPool, testEnvServerPool = serverPool}
    liftIO $ func tenv
