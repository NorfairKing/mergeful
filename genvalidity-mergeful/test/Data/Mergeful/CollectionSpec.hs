{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Data.Mergeful.CollectionSpec
  ( spec
  ) where

import Data.Functor.Identity
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.UUID
import GHC.Generics (Generic)
import System.Random
import Text.Show.Pretty

import Control.Monad.State

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

import Data.GenValidity.UUID ()

import Data.Mergeful
import Data.Mergeful.Timed

import Data.GenValidity.Mergeful ()
import Data.GenValidity.Mergeful.Item ()

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
  genValidSpec @ClientId
  jsonSpecOnValid @ClientId
  genValidSpec @(ClientStore Int Int)
  jsonSpecOnValid @(ClientStore Int Int)
  genValidSpec @(ServerStore Int Int)
  jsonSpecOnValid @(ServerStore Int Int)
  genValidSpec @(SyncRequest Int Int)
  jsonSpecOnValid @(SyncRequest Int Int)
  genValidSpec @(ClientAddition Int)
  jsonSpecOnValid @(ClientAddition Int)
  genValidSpec @(SyncResponse Int Int)
  jsonSpecOnValid @(SyncResponse Int Int)
  describe "initialClientStore" $ it "is valid" $ shouldBeValid $ initialClientStore @Int @Int
  describe "addItemToClientStore" $ do
    it "produces valid stores" $ producesValidsOnValids2 (addItemToClientStore @Int @Int)
    it "makes the client store one bigger" $
      forAllValid $ \cs ->
        forAllValid $ \a ->
          clientStoreSize @Int @Int (addItemToClientStore a cs) `shouldBe` (clientStoreSize cs + 1)
    it "ensures that the added item is in fact in the result" $
      forAllValid $ \cs ->
        forAllValid $ \a ->
          let cs' = addItemToClientStore @Int @Int a cs
           in a `elem` clientStoreAddedItems cs'
  describe "markItemDeletedInClientStore" $ do
    it "produces valid stores" $ producesValidsOnValids2 (markItemDeletedInClientStore @Int @Int)
    it "makes the client store one smaller" $
      let p cs = S.size (clientStoreUndeletedSyncIdSet cs) > 0
       in forAllShrink (genValid `suchThat` p) (filter p . shrinkValid) $ \cs ->
            forAll (elements $ S.toList $ clientStoreUndeletedSyncIdSet cs) $ \i ->
              (clientStoreSize @Int @Int (markItemDeletedInClientStore i cs) + 1) `shouldBe`
              clientStoreSize cs
    it "ensures that the added item is in fact not in the result" $
      let p cs = S.size (clientStoreUndeletedSyncIdSet cs) > 0
       in forAllShrink (genValid `suchThat` p) (filter p . shrinkValid) $ \cs ->
            forAll (elements $ S.toList $ clientStoreUndeletedSyncIdSet cs) $ \i ->
              let cs' = markItemDeletedInClientStore @Int @Int i cs
               in M.lookup (Right i) (clientStoreItems cs') `shouldBe` Nothing
  describe "changeItemInClientStore" $ do
    it "produces valid stores" $ producesValidsOnValids3 (changeItemInClientStore @Int @Int)
    it "doesn't change the size of the client store" $
      let p cs = S.size (clientStoreUndeletedSyncIdSet cs) > 0
       in forAllShrink (genValid `suchThat` p) (filter p . shrinkValid) $ \cs ->
            forAll (elements $ S.toList $ clientStoreUndeletedSyncIdSet cs) $ \i ->
              forAllValid $ \a ->
                clientStoreSize @Int @Int (changeItemInClientStore i a cs) `shouldBe`
                clientStoreSize cs
    it "ensures that the changed item is in fact in the result" $
      let p cs = S.size (clientStoreUndeletedSyncIdSet cs) > 0
       in forAllShrink (genValid `suchThat` p) (filter p . shrinkValid) $ \cs ->
            forAll (elements $ S.toList $ clientStoreUndeletedSyncIdSet cs) $ \i ->
              forAllValid $ \a ->
                let cs' = changeItemInClientStore @Int @Int i a cs
                 in M.lookup (Right i) (clientStoreItems cs') `shouldBe` Just a
  describe "deleteItemFromClientStore" $ do
    it "produces valid stores" $ producesValidsOnValids2 (deleteItemFromClientStore @Int @Int)
    it "makes the client store one smaller" $
      let p cs = S.size (clientStoreClientIdSet cs) > 0
       in forAllShrink (genValid `suchThat` p) (filter p . shrinkValid) $ \cs ->
            forAll (elements $ S.toList $ clientStoreClientIdSet cs) $ \cid ->
              (clientStoreSize @Int @Int (deleteItemFromClientStore cid cs) + 1) `shouldBe`
              clientStoreSize cs
  describe "initialServerStore" $ it "is valid" $ shouldBeValid $ initialServerStore @Int @Int
  describe "initialSyncRequest" $ it "is valid" $ shouldBeValid $ initialSyncRequest @Int @Int
  describe "emptySyncResponse" $ it "is valid" $ shouldBeValid $ emptySyncResponse @Int @Int
  describe "makeSyncRequest" $
    it "produces valid requests" $ producesValidsOnValids (makeSyncRequest @Int @Int)
  describe "mergeAddedItems" $
    it "produces valid results" $ producesValidsOnValids2 (mergeAddedItems @Int @Int)
  describe "mergeSyncedButChangedItems" $
    it "produces valid results" $ producesValidsOnValids2 (mergeSyncedButChangedItems @Int @Int)
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
  describe "processServerSync" $
    it "produces valid tuples of a response and a store" $
    producesValidsOnValids2
      (\store request -> evalD $ processServerSync genD (store :: ServerStore UUID Int) request)
  describe "Syncing with mergeSyncResponseIgnoreProblems" $ do
    mergeFunctionSpec @Int mergeSyncResponseIgnoreProblems
    it "does not lose data after a conflict occurs" $
      forAllValid $ \uuid ->
        forAllValid $ \time1 ->
          forAllValid $ \i1 ->
            forAllValid $ \i2 ->
              forAllValid $ \i3 ->
                evalDM $ do
                  let sstore1 =
                        ServerStore
                          {serverStoreItems = M.singleton (uuid :: UUID) (Timed (i1 :: Int) time1)}
                      -- The server has an item
                      -- The first client has synced it, and modified it.
                  let cAstore1 =
                        initialClientStore
                          {clientStoreSyncedButChangedItems = M.singleton uuid (Timed i2 time1)}
                      -- The second client has synced it too, and modified it too.
                  let cBstore1 =
                        initialClientStore
                          {clientStoreSyncedButChangedItems = M.singleton uuid (Timed i3 time1)}
                      -- Client A makes sync request 1
                  let req1 = makeSyncRequest cAstore1
                      -- The server processes sync request 1
                  (resp1, sstore2) <- processServerSync genD sstore1 req1
                  let time2 = incrementServerTime time1
                      -- The server updates the item accordingly
                  lift $ do
                    resp1 `shouldBe`
                      (emptySyncResponse {syncResponseClientChanged = M.singleton uuid time2})
                    sstore2 `shouldBe`
                      (ServerStore {serverStoreItems = M.singleton uuid (Timed i2 time2)})
                      -- Client A merges the response
                  let cAstore2 = mergeSyncResponseIgnoreProblems cAstore1 resp1
                  lift $
                    cAstore2 `shouldBe`
                    (initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed i2 time2)})
                      -- Client B makes sync request 2
                  let req2 = makeSyncRequest cBstore1
                      -- The server processes sync request 2
                  (resp2, sstore3) <- processServerSync genD sstore2 req2
                      -- The server reports a conflict and does not change its store
                  lift $ do
                    resp2 `shouldBe`
                      (emptySyncResponse {syncResponseConflicts = M.singleton uuid (Timed i2 time2)})
                    sstore3 `shouldBe` sstore2
                      -- Client B merges the response
                  let cBstore2 = mergeSyncResponseIgnoreProblems cBstore1 resp2
                      -- Client does not update, but keeps its conflict
                  lift $
                    cBstore2 `shouldBe`
                    (initialClientStore
                       {clientStoreSyncedButChangedItems = M.singleton uuid (Timed i3 time1)})
                      -- Client A and Client B now *do not* have the same store
  describe "Syncing with mergeSyncResponseFromServer" $ do
    mergeFunctionSpec @Int mergeSyncResponseFromServer
    describe "mergeSyncResponseFromServer" $
      it "only differs from mergeSyncResponseIgnoreProblems on conflicts" $
      forAllValid $ \cstore ->
        forAllValid $ \sresp@SyncResponse {..} -> do
          let cstoreA = mergeSyncResponseFromServer (cstore :: ClientStore UUID Int) sresp
              cstoreB = mergeSyncResponseIgnoreProblems cstore sresp
          if cstoreA == cstoreB
            then pure ()
            else unless
                   (or
                      [ not (M.null syncResponseConflicts)
                      , not (M.null syncResponseConflictsClientDeleted)
                      , not (S.null syncResponseConflictsServerDeleted)
                      ]) $
                 expectationFailure $
                 unlines
                   [ "There was a difference between mergeSyncResponseFromServer and mergeSyncResponseIgnoreProblems that was somehow unrelated to the conflicts:"
                   , "syncResponseConflicts:"
                   , ppShow syncResponseConflicts
                   , "syncResponseConflictsClientDeleted:"
                   , ppShow syncResponseConflictsClientDeleted
                   , "syncResponseConflictsServerDeleted:"
                   , ppShow syncResponseConflictsServerDeleted
                   , "client store after mergeSyncResponseFromServer:"
                   , ppShow cstoreA
                   , "client store after mergeSyncResponseIgnoreProblems:"
                   , ppShow cstoreB
                   ]
    it "does not diverge after a conflict occurs" $
      forAllValid $ \uuid ->
        forAllValid $ \time1 ->
          forAllValid $ \i1 ->
            forAllValid $ \i2 ->
              forAllValid $ \i3 ->
                evalDM $ do
                  let sstore1 =
                        ServerStore
                          {serverStoreItems = M.singleton (uuid :: UUID) (Timed (i1 :: Int) time1)}
                      -- The server has an item
                      -- The first client has synced it, and modified it.
                  let cAstore1 =
                        initialClientStore
                          {clientStoreSyncedButChangedItems = M.singleton uuid (Timed i2 time1)}
                      -- The second client has synced it too, and modified it too.
                  let cBstore1 =
                        initialClientStore
                          {clientStoreSyncedButChangedItems = M.singleton uuid (Timed i3 time1)}
                      -- Client A makes sync request 1
                  let req1 = makeSyncRequest cAstore1
                      -- The server processes sync request 1
                  (resp1, sstore2) <- processServerSync genD sstore1 req1
                  let time2 = incrementServerTime time1
                      -- The server updates the item accordingly
                  lift $ do
                    resp1 `shouldBe`
                      (emptySyncResponse {syncResponseClientChanged = M.singleton uuid time2})
                    sstore2 `shouldBe`
                      (ServerStore {serverStoreItems = M.singleton uuid (Timed i2 time2)})
                      -- Client A merges the response
                  let cAstore2 = mergeSyncResponseFromServer cAstore1 resp1
                  lift $
                    cAstore2 `shouldBe`
                    (initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed i2 time2)})
                      -- Client B makes sync request 2
                  let req2 = makeSyncRequest cBstore1
                      -- The server processes sync request 2
                  (resp2, sstore3) <- processServerSync genD sstore2 req2
                      -- The server reports a conflict and does not change its store
                  lift $ do
                    resp2 `shouldBe`
                      (emptySyncResponse {syncResponseConflicts = M.singleton uuid (Timed i2 time2)})
                    sstore3 `shouldBe` sstore2
                      -- Client B merges the response
                  let cBstore2 = mergeSyncResponseFromServer cBstore1 resp2
                      -- Client does not update, but keeps its conflict
                  lift $ do
                    cBstore2 `shouldBe`
                      (initialClientStore
                         {clientStoreSyncedItems = M.singleton uuid (Timed i2 time2)})
                        -- Client A and Client B now have the same store
                    cBstore2 `shouldBe` cAstore2
  describe "Syncing with mergeSyncResponseUsingStrategy with a GCounter" $
    mergeFunctionSpec @Int $
    mergeSyncResponseUsingStrategy
      ItemMergeStrategy
        { itemMergeStrategyMergeChangeConflict =
            \clientItem (Timed serverItem t) -> Timed (max clientItem serverItem) t
        , itemMergeStrategyMergeClientDeletedConflict = \serverItem -> Just serverItem
        , itemMergeStrategyMergeServerDeletedConflict = \_ -> Nothing
        }

mergeFunctionSpec ::
     forall a. (Show a, Ord a, GenValid a)
  => (forall i. Ord i =>
                  ClientStore i a -> SyncResponse i a -> ClientStore i a)
  -> Spec
mergeFunctionSpec mergeFunc = do
  describe "Single client" $
    describe "Multi-item" $ do
      it "succesfully downloads everything from the server for an empty client" $
        forAllValid $ \sstore1 ->
          evalDM $ do
            let cstore1 = initialClientStore
            let req = makeSyncRequest cstore1
            (resp, sstore2) <- processServerSync genD sstore1 req
            let cstore2 = mergeFunc cstore1 resp
            lift $ do
              sstore2 `shouldBe` sstore1
              clientStoreSyncedItems cstore2 `shouldBe` serverStoreItems sstore2
      it "succesfully uploads everything to the server for an empty server" $
        forAllValid $ \items ->
          evalDM $ do
            let cstore1 = initialClientStore {clientStoreAddedItems = items}
            let sstore1 = initialServerStore
            let req = makeSyncRequest cstore1
            (resp, sstore2) <- processServerSync genD sstore1 req
            let cstore2 = mergeFunc cstore1 resp
            lift $ do
              sort (M.elems (M.map timedValue (clientStoreSyncedItems cstore2))) `shouldBe`
                sort (M.elems items)
              clientStoreSyncedItems cstore2 `shouldBe` serverStoreItems sstore2
      it "is idempotent with one client" $
        forAllValid $ \cstore1 ->
          forAllValid $ \sstore1 ->
            evalDM $ do
              let req1 = makeSyncRequest cstore1
              (resp1, sstore2) <- processServerSync genD sstore1 req1
              let cstore2 = mergeFunc cstore1 resp1
                  req2 = makeSyncRequest cstore2
              (resp2, sstore3) <- processServerSync genD sstore2 req2
              let cstore3 = mergeFunc cstore2 resp2
              lift $ do
                cstore2 `shouldBe` cstore3
                sstore2 `shouldBe` sstore3
  describe "Multiple clients" $ do
    describe "Single-item" $ do
      it "successfully syncs an addition accross to a second client" $
        forAllValid $ \i ->
          evalDM $ do
            let cAstore1 = initialClientStore {clientStoreAddedItems = M.singleton (ClientId 0) i}
              -- Client B is empty
            let cBstore1 = initialClientStore
              -- The server is empty
            let sstore1 = initialServerStore
              -- Client A makes sync request 1
            let req1 = makeSyncRequest cAstore1
              -- The server processes sync request 1
            (resp1, sstore2) <- processServerSync genD sstore1 req1
            let addedItems = syncResponseClientAdded resp1
            case M.toList addedItems of
              [(ClientId 0, ClientAddition uuid st)] -> do
                let time = initialServerTime
                lift $ st `shouldBe` time
                let items = M.singleton uuid (Timed i st)
                lift $ sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
                  -- Client A merges the response
                let cAstore2 = mergeFunc cAstore1 resp1
                lift $ cAstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
                  -- Client B makes sync request 2
                let req2 = makeSyncRequest cBstore1
                  -- The server processes sync request 2
                (resp2, sstore3) <- processServerSync genD sstore2 req2
                lift $ do
                  resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
                  sstore3 `shouldBe` sstore2
                  -- Client B merges the response
                let cBstore2 = mergeFunc cBstore1 resp2
                lift $ cBstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
                  -- Client A and Client B now have the same store
                lift $ cAstore2 `shouldBe` cBstore2
              _ -> lift $ expectationFailure "Should have found exactly one added item."
      it "successfully syncs a modification accross to a second client" $
        forAllValid $ \uuid ->
          forAllValid $ \i ->
            forAllValid $ \j ->
              forAllValid $ \time1 ->
                evalDM $ do
                  let cAstore1 =
                        initialClientStore
                          {clientStoreSyncedItems = M.singleton uuid (Timed i time1)}
                    -- Client B had synced that same item, but has since modified it
                  let cBstore1 =
                        initialClientStore
                          {clientStoreSyncedButChangedItems = M.singleton uuid (Timed j time1)}
                    -- The server is has the item that both clients had before
                  let sstore1 = ServerStore {serverStoreItems = M.singleton uuid (Timed i time1)}
                    -- Client B makes sync request 1
                  let req1 = makeSyncRequest cBstore1
                    -- The server processes sync request 1
                  (resp1, sstore2) <- processServerSync genD sstore1 req1
                  let time2 = incrementServerTime time1
                  lift $ do
                    resp1 `shouldBe`
                      emptySyncResponse {syncResponseClientChanged = M.singleton uuid time2}
                    sstore2 `shouldBe`
                      ServerStore {serverStoreItems = M.singleton uuid (Timed j time2)}
                    -- Client B merges the response
                  let cBstore2 = mergeFunc cBstore1 resp1
                  lift $
                    cBstore2 `shouldBe`
                    initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed j time2)}
                    -- Client A makes sync request 2
                  let req2 = makeSyncRequest cAstore1
                    -- The server processes sync request 2
                  (resp2, sstore3) <- processServerSync genD sstore2 req2
                  lift $ do
                    resp2 `shouldBe`
                      emptySyncResponse
                        {syncResponseServerChanged = M.singleton uuid (Timed j time2)}
                    sstore3 `shouldBe` sstore2
                    -- Client A merges the response
                  let cAstore2 = mergeFunc cAstore1 resp2
                  lift $
                    cAstore2 `shouldBe`
                    initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed j time2)}
                    -- Client A and Client B now have the same store
                  lift $ cAstore2 `shouldBe` cBstore2
      it "succesfully syncs a deletion across to a second client" $
        forAllValid $ \uuid ->
          forAllValid $ \time1 ->
            forAllValid $ \i ->
              evalDM $ do
                let cAstore1 =
                      initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed i time1)}
                  -- Client A has a synced item.
                  -- Client B had synced that same item, but has since deleted it.
                let cBstore1 = initialClientStore {clientStoreDeletedItems = M.singleton uuid time1}
                  -- The server still has the undeleted item
                let sstore1 = ServerStore {serverStoreItems = M.singleton uuid (Timed i time1)}
                  -- Client B makes sync request 1
                let req1 = makeSyncRequest cBstore1
                  -- The server processes sync request 1
                (resp1, sstore2) <- processServerSync genD sstore1 req1
                lift $ do
                  resp1 `shouldBe` emptySyncResponse {syncResponseClientDeleted = S.singleton uuid}
                  sstore2 `shouldBe` initialServerStore
                  -- Client B merges the response
                let cBstore2 = mergeFunc cBstore1 resp1
                lift $ cBstore2 `shouldBe` initialClientStore
                  -- Client A makes sync request 2
                let req2 = makeSyncRequest cAstore1
                  -- The server processes sync request 2
                (resp2, sstore3) <- processServerSync genD sstore2 req2
                lift $ do
                  resp2 `shouldBe` emptySyncResponse {syncResponseServerDeleted = S.singleton uuid}
                  sstore3 `shouldBe` sstore2
                  -- Client A merges the response
                let cAstore2 = mergeFunc cAstore1 resp2
                lift $ cAstore2 `shouldBe` initialClientStore
                  -- Client A and Client B now have the same store
                lift $ cAstore2 `shouldBe` cBstore2
      it "does not run into a conflict if two clients both try to sync a deletion" $
        forAllValid $ \uuid ->
          forAllValid $ \time1 ->
            forAllValid $ \i ->
              evalDM $ do
                let cAstore1 = initialClientStore {clientStoreDeletedItems = M.singleton uuid time1}
                  -- Both client a and client b delete an item.
                let cBstore1 = initialClientStore {clientStoreDeletedItems = M.singleton uuid time1}
                  -- The server still has the undeleted item
                let sstore1 = ServerStore {serverStoreItems = M.singleton uuid (Timed i time1)}
                  -- Client A makes sync request 1
                let req1 = makeSyncRequest cAstore1
                  -- The server processes sync request 1
                (resp1, sstore2) <- processServerSync genD sstore1 req1
                lift $ do
                  resp1 `shouldBe`
                    (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
                  sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty})
                  -- Client A merges the response
                let cAstore2 = mergeFunc cAstore1 resp1
                lift $ cAstore2 `shouldBe` initialClientStore
                  -- Client B makes sync request 2
                let req2 = makeSyncRequest cBstore1
                  -- The server processes sync request 2
                (resp2, sstore3) <- processServerSync genD sstore2 req2
                lift $ do
                  resp2 `shouldBe`
                    (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
                  sstore3 `shouldBe` sstore2
                  -- Client B merges the response
                let cBstore2 = mergeFunc cBstore1 resp2
                lift $ do
                  cBstore2 `shouldBe` initialClientStore
                    -- Client A and Client B now have the same store
                  cAstore2 `shouldBe` cBstore2
    describe "Multiple items" $ do
      it "successfully syncs additions accross to a second client" $
        forAllValid $ \is ->
          evalDM $ do
            let cAstore1 = initialClientStore {clientStoreAddedItems = is}
              -- Client B is empty
            let cBstore1 = initialClientStore
              -- The server is empty
            let sstore1 = initialServerStore
              -- Client A makes sync request 1
            let req1 = makeSyncRequest cAstore1
              -- The server processes sync request 1
            (resp1, sstore2) <- processServerSync genD sstore1 req1
            let (rest, items) = mergeAddedItems is (syncResponseClientAdded resp1)
            lift $ do
              rest `shouldBe` M.empty
              sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
              -- Client A merges the response
            let cAstore2 = mergeFunc cAstore1 resp1
            lift $ cAstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
              -- Client B makes sync request 2
            let req2 = makeSyncRequest cBstore1
              -- The server processes sync request 2
            (resp2, sstore3) <- processServerSync genD sstore2 req2
            lift $ do
              resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
              sstore3 `shouldBe` sstore2
              -- Client B merges the response
            let cBstore2 = mergeFunc cBstore1 resp2
            lift $ cBstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
              -- Client A and Client B now have the same store
            lift $ cAstore2 `shouldBe` cBstore2
      it "succesfully syncs deletions across to a second client" $
        forAllValid $ \items ->
          forAllValid $ \time1 ->
            evalDM $ do
              let syncedItems = M.map (\i -> Timed i time1) items
                  itemTimes = M.map (const time1) items
                  itemIds = M.keysSet items
              let cAstore1 = initialClientStore {clientStoreSyncedItems = syncedItems}
                -- Client A has synced items
                -- Client B had synced the same items, but has since deleted them.
              let cBstore1 = initialClientStore {clientStoreDeletedItems = itemTimes}
                -- The server still has the undeleted item
              let sstore1 = ServerStore {serverStoreItems = syncedItems}
                -- Client B makes sync request 1
              let req1 = makeSyncRequest cBstore1
                -- The server processes sync request 1
              (resp1, sstore2) <- processServerSync genD sstore1 req1
              lift $ do
                resp1 `shouldBe` emptySyncResponse {syncResponseClientDeleted = itemIds}
                sstore2 `shouldBe` initialServerStore
                -- Client B merges the response
              let cBstore2 = mergeFunc cBstore1 resp1
              lift $ cBstore2 `shouldBe` initialClientStore
                -- Client A makes sync request 2
              let req2 = makeSyncRequest cAstore1
                -- The server processes sync request 2
              (resp2, sstore3) <- processServerSync genD sstore2 req2
              lift $ do
                resp2 `shouldBe` emptySyncResponse {syncResponseServerDeleted = itemIds}
                sstore3 `shouldBe` sstore2
                -- Client A merges the response
              let cAstore2 = mergeFunc cAstore1 resp2
              lift $ cAstore2 `shouldBe` initialClientStore
                -- Client A and Client B now have the same store
              lift $ cAstore2 `shouldBe` cBstore2
      it "does not run into a conflict if two clients both try to sync a deletion" $
        forAllValid $ \items ->
          forAllValid $ \time1 ->
            evalDM $ do
              let cAstore1 =
                    initialClientStore {clientStoreDeletedItems = M.map (const time1) items}
                -- Both client a and client b delete their items.
              let cBstore1 =
                    initialClientStore {clientStoreDeletedItems = M.map (const time1) items}
                -- The server still has the undeleted items
              let sstore1 = ServerStore {serverStoreItems = M.map (\i -> Timed i time1) items}
                -- Client A makes sync request 1
              let req1 = makeSyncRequest cAstore1
                -- The server processes sync request 1
              (resp1, sstore2) <- processServerSync genD sstore1 req1
              lift $ do
                resp1 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = M.keysSet items})
                sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty}) -- TODO will probably need some sort of tombstoning.
                -- Client A merges the response
              let cAstore2 = mergeFunc cAstore1 resp1
              lift $ cAstore2 `shouldBe` initialClientStore
                -- Client B makes sync request 2
              let req2 = makeSyncRequest cBstore1
                -- The server processes sync request 2
              (resp2, sstore3) <- processServerSync genD sstore2 req2
              lift $ do
                resp2 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = M.keysSet items})
                sstore3 `shouldBe` sstore2
                -- Client B merges the response
              let cBstore2 = mergeFunc cBstore1 resp2
              lift $ do
                cBstore2 `shouldBe` initialClientStore
                  -- Client A and Client B now have the same store
                cAstore2 `shouldBe` cBstore2

newtype D m a =
  D
    { unD :: StateT StdGen m a
    }
  deriving (Generic, Functor, Applicative, Monad, MonadState StdGen, MonadTrans, MonadIO)

evalD :: D Identity a -> a
evalD = runIdentity . evalDM

-- runD :: D Identity a -> StdGen -> (a, StdGen)
-- runD = runState . unD
evalDM :: Functor m => D m a -> m a
evalDM d = fst <$> runDM d (mkStdGen 42)

runDM :: D m a -> StdGen -> m (a, StdGen)
runDM = runStateT . unD

genD :: Monad m => D m UUID
genD = do
  r <- get
  let (u, r') = random r
  put r'
  pure u