{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.MergefulSpec
  ( spec
  ) where

import Debug.Trace

import Data.Functor
import Data.Functor.Identity
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.UUID.Typed as Typed
import GHC.Generics (Generic)
import System.Random
import Text.Show.Pretty

import Control.Monad.State

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Data.GenValidity.UUID.Typed ()

import Data.Mergeful
import Data.Mergeful.Item

import Data.GenValidity.Mergeful ()
import Data.GenValidity.Mergeful.Item

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
  genValidSpec @(Timed Int)
  genValidSpec @(ClientStore Int Int)
  genValidSpec @(ServerStore Int Int)
  genValidSpec @(SyncRequest Int Int)
  genValidSpec @(SyncResponse Int Int)
  describe "emptySyncResponse" $ it "is valid" $ shouldBeValid $ emptySyncResponse @Int @Int
  describe "makeSyncRequest" $
    it "produces valid requests" $ producesValidsOnValids (makeSyncRequest @Int @Int)
  describe "mergeAddedItems" $
    it "produces valid results" $ producesValidsOnValids2 (mergeAddedItems @Int @Int)
  describe "mergeSyncedButChangedItems" $
    it "produces valid results" $ producesValidsOnValids2 (mergeSyncedButChangedItems @Int @Int)
  describe "mergeDeletedItems" $
    it "produces valid results" $ producesValidsOnValids2 (mergeDeletedItems @Int @Int)
  describe "mergeSyncResponseIgnoreProblems" $
    it "produces valid requests" $
    forAllValid $ \store ->
      forAllValid $ \response ->
        let res = mergeSyncResponseIgnoreProblems @Int @Int store response
         in case prettyValidate res of
              Right _ -> pure ()
              Left err ->
                expectationFailure $
                unlines
                  [ "Store:"
                  , ppShow store
                  , "Response:"
                  , ppShow response
                  , "Invalid result:"
                  , ppShow res
                  , "error:"
                  , err
                  ]
  describe "processServerSync" $ do
    it "produces valid tuples of a response and a store" $
      producesValidsOnValids2
        (\store request ->
           evalD $ processServerSync genD (store :: ServerStore (UUID Int) Int) request)
    describe "Single client" $ do
      describe "Multi-item" $ do
        it "succesfully downloads everything from the server for an empty client" $
          forAllValid $ \sstore1 ->
            evalDM $ do
              let cstore1 = emptyClientStore :: ClientStore (UUID Int) Int
              let req = makeSyncRequest cstore1
              (resp, sstore2) <- processServerSync genD sstore1 req
              let cstore2 = mergeSyncResponseIgnoreProblems cstore1 resp
              lift $ do
                sstore2 `shouldBe` sstore1
                clientStoreSyncedItems cstore2 `shouldBe` serverStoreItems sstore2
        it "succesfully uploads everything to the server for an empty server" $
          forAllValid $ \items ->
            evalDM $ do
              let cstore1 = emptyClientStore {clientStoreAddedItems = items}
              let sstore1 = emptyServerStore :: ServerStore (UUID Int) Int
              let req = makeSyncRequest cstore1
              (resp, sstore2) <- processServerSync genD sstore1 req
              let cstore2 = mergeSyncResponseIgnoreProblems cstore1 resp
              lift $ do
                sort (M.elems (M.map timedValue (clientStoreSyncedItems cstore2))) `shouldBe`
                  sort items
                clientStoreSyncedItems cstore2 `shouldBe` serverStoreItems sstore2
        it "is idempotent with one client" $
          forAllValid $ \cstore1 ->
            forAllValid $ \sstore1 ->
              evalDM $ do
                let req1 = makeSyncRequest (cstore1 :: ClientStore (UUID Int) Int)
                (resp1, sstore2) <- processServerSync genD sstore1 req1
                let cstore2 = mergeSyncResponseIgnoreProblems cstore1 resp1
                    req2 = makeSyncRequest cstore2
                (resp2, sstore3) <- processServerSync genD sstore2 req2
                let cstore3 = mergeSyncResponseIgnoreProblems cstore2 resp2
                lift $ do
                  cstore2 `shouldBe` cstore3
                  sstore2 `shouldBe` sstore3
    describe "Multiple clients" $ do
      describe "Single-item" $ do
        it "successfully syncs an addition accross to a second client" $
          forAllValid $ \i ->
            evalDM $ do
              let cAstore1 = emptyClientStore {clientStoreAddedItems = [i]}
              -- Client B is empty
              let cBstore1 = emptyClientStore :: ClientStore (UUID Int) Int
              -- The server is empty
              let sstore1 = emptyServerStore
              -- Client A makes sync request 1
              let req1 = makeSyncRequest cAstore1
              -- The server processes sync request 1
              (resp1, sstore2) <- processServerSync genD sstore1 req1
              let time = initialServerTime
              let addedItems = syncResponseAddedItems resp1
              case M.toList addedItems of
                [(0, (uuid, st))] -> do
                  lift $ st `shouldBe` time
                  let items = M.singleton uuid (Timed i st)
                  lift $ sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
                  -- Client A merges the response
                  let cAstore2 = mergeSyncResponseIgnoreProblems cAstore1 resp1
                  lift $ cAstore2 `shouldBe` (emptyClientStore {clientStoreSyncedItems = items})
                  -- Client B makes sync request 2
                  let req2 = makeSyncRequest cBstore1
                  -- The server processes sync request 2
                  (resp2, sstore3) <- processServerSync genD sstore2 req2
                  lift $ do
                    resp2 `shouldBe` (emptySyncResponse {syncResponseNewRemoteItems = items})
                    sstore3 `shouldBe` sstore2
                  -- Client B merges the response
                  let cBstore2 = mergeSyncResponseIgnoreProblems cBstore1 resp2
                  lift $ cBstore2 `shouldBe` (emptyClientStore {clientStoreSyncedItems = items})
                  -- Client A and Client B now have the same store
                  lift $ cAstore2 `shouldBe` cBstore2
                _ -> lift $ expectationFailure "Should have found exactly one added item."
        it "does not run into a conflict if two clients both try to sync a deletion" $
          forAllValid $ \uuid ->
            forAllValid $ \time1 ->
              forAllValid $ \i ->
                evalDM $ do
                  let cAstore1 =
                        emptyClientStore
                          {clientStoreDeletedItems = M.singleton (uuid :: UUID Int) time1}
                  -- Both client a and client b delete an item.
                  let cBstore1 = emptyClientStore {clientStoreDeletedItems = M.singleton uuid time1}
                  -- The server still has the undeleted item
                  let sstore1 =
                        ServerStore {serverStoreItems = M.singleton uuid (Timed (i :: Int) time1)}
                  -- Client A makes sync request 1
                  let req1 = makeSyncRequest cAstore1
                  -- The server processes sync request 1
                  (resp1, sstore2) <- processServerSync genD sstore1 req1
                  let time2 = incrementServerTime time1
                  lift $ do
                    resp1 `shouldBe`
                      (emptySyncResponse {syncResponseItemsToBeDeletedLocally = S.singleton uuid})
                    sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty}) -- TODO will probably need some sort of tombstoning.
                  -- Client A merges the response
                  let cAstore2 = mergeSyncResponseIgnoreProblems cAstore1 resp1
                  lift $ cAstore2 `shouldBe` emptyClientStore
                  -- Client B makes sync request 2
                  let req2 = makeSyncRequest cBstore1
                  -- The server processes sync request 2
                  (resp2, sstore3) <- processServerSync genD sstore2 req2
                  lift $ do
                    resp2 `shouldBe`
                      (emptySyncResponse {syncResponseItemsToBeDeletedLocally = S.singleton uuid})
                    sstore3 `shouldBe` sstore2
                  -- Client B merges the response
                  let cBstore2 = mergeSyncResponseIgnoreProblems cBstore1 resp2
                  lift $ do
                    cBstore2 `shouldBe` emptyClientStore
                    -- Client A and Client B now have the same store
                    cAstore2 `shouldBe` cBstore2
      describe "Multiple items" $ do
        it "successfully syncs additions accross to a second client" $
          forAllValid $ \is ->
            evalDM $ do
              let cAstore1 = emptyClientStore {clientStoreAddedItems = is}
              -- Client B is empty
              let cBstore1 = emptyClientStore :: ClientStore (UUID Int) Int
              -- The server is empty
              let sstore1 = emptyServerStore
              -- Client A makes sync request 1
              let req1 = makeSyncRequest cAstore1
              -- The server processes sync request 1
              (resp1, sstore2) <- processServerSync genD sstore1 req1
              let time = initialServerTime
              let (rest, items) = mergeAddedItems (addedItemsIntmap is) (syncResponseAddedItems resp1)
              lift $ do
                rest `shouldBe` []
                sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
              -- Client A merges the response
              let cAstore2 = mergeSyncResponseIgnoreProblems cAstore1 resp1
              lift $ cAstore2 `shouldBe` (emptyClientStore {clientStoreSyncedItems = items})
              -- Client B makes sync request 2
              let req2 = makeSyncRequest cBstore1
              -- The server processes sync request 2
              (resp2, sstore3) <- processServerSync genD sstore2 req2
              lift $ do
                resp2 `shouldBe` (emptySyncResponse {syncResponseNewRemoteItems = items})
                sstore3 `shouldBe` sstore2
              -- Client B merges the response
              let cBstore2 = mergeSyncResponseIgnoreProblems cBstore1 resp2
              lift $ cBstore2 `shouldBe` (emptyClientStore {clientStoreSyncedItems = items})
              -- Client A and Client B now have the same store
              lift $ cAstore2 `shouldBe` cBstore2
        it "does not run into a conflict if two clients both try to sync a deletion" $
          forAllValid $ \items ->
            forAllValid $ \time1 ->
              evalDM $ do
                let cAstore1 =
                      emptyClientStore
                        { clientStoreDeletedItems =
                            M.map (const time1) (items :: Map (UUID Int) Int)
                        }
                -- Both client a and client b delete their items.
                let cBstore1 =
                      emptyClientStore {clientStoreDeletedItems = M.map (const time1) items}
                -- The server still has the undeleted items
                let sstore1 = ServerStore {serverStoreItems = M.map (\i -> Timed i time1) items}
                -- Client A makes sync request 1
                let req1 = makeSyncRequest cAstore1
                -- The server processes sync request 1
                (resp1, sstore2) <- processServerSync genD sstore1 req1
                let time2 = incrementServerTime time1
                lift $ do
                  resp1 `shouldBe`
                    (emptySyncResponse {syncResponseItemsToBeDeletedLocally = M.keysSet items})
                  sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty}) -- TODO will probably need some sort of tombstoning.
                -- Client A merges the response
                let cAstore2 = mergeSyncResponseIgnoreProblems cAstore1 resp1
                lift $ cAstore2 `shouldBe` emptyClientStore
                -- Client B makes sync request 2
                let req2 = makeSyncRequest cBstore1
                -- The server processes sync request 2
                (resp2, sstore3) <- processServerSync genD sstore2 req2
                lift $ do
                  resp2 `shouldBe`
                    (emptySyncResponse {syncResponseItemsToBeDeletedLocally = M.keysSet items})
                  sstore3 `shouldBe` sstore2
                -- Client B merges the response
                let cBstore2 = mergeSyncResponseIgnoreProblems cBstore1 resp2
                lift $ do
                  cBstore2 `shouldBe` emptyClientStore
                  -- Client A and Client B now have the same store
                  cAstore2 `shouldBe` cBstore2

newtype D m a =
  D
    { unD :: StateT StdGen m a
    }
  deriving (Generic, Functor, Applicative, Monad, MonadState StdGen, MonadTrans)

evalD :: D Identity a -> a
evalD = runIdentity . evalDM

runD :: D Identity a -> StdGen -> (a, StdGen)
runD = runState . unD

evalDM :: Functor m => D m a -> m a
evalDM d = fst <$> runDM d (mkStdGen 42)

runDM :: D m a -> StdGen -> m (a, StdGen)
runDM = runStateT . unD

genD :: Monad m => D m (Typed.UUID a)
genD = do
  r <- get
  let (u, r') = random r
  put r'
  pure u
