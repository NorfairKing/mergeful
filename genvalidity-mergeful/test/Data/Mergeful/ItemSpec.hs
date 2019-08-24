{-# LANGUAGE TypeApplications #-}

module Data.Mergeful.ItemSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

import Data.Mergeful.Item
import Data.Mergeful.Timed


import Data.GenValidity.Mergeful.Item ()

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

forAllSubsequent :: Testable prop => ((ServerTime, ServerTime) -> prop) -> Property
forAllSubsequent func =
  forAllValid $ \st ->
    forAllShrink (genValid `suchThat` (> st)) (filter (> st) . shrinkValid) $ \st' -> func (st, st')

spec :: Spec
spec = do
  genValidSpec @ServerTime
  jsonSpecOnValid @ServerTime
  genValidSpec @(Timed Int)
  jsonSpecOnValid @(Timed Int)
  genValidSpec @(ClientItem Int)
  jsonSpecOnValid @(ClientItem Int)
  genValidSpec @(ServerItem Int)
  jsonSpecOnValid @(ServerItem Int)
  genValidSpec @(ItemSyncRequest Int)
  jsonSpecOnValid @(ItemSyncRequest Int)
  genValidSpec @(ItemSyncResponse Int)
  jsonSpecOnValid @(ItemSyncResponse Int)
  describe "makeItemSyncRequest" $
    it "produces valid requests" $ producesValidsOnValids (makeItemSyncRequest @Int)
  describe "mergeItemSyncResponseRaw" $
    it "produces valid client stores" $ producesValidsOnValids2 (mergeItemSyncResponseRaw @Int)
  describe "mergeItemSyncResponseIgnoreProblems" $
    it "produces valid client stores" $
    producesValidsOnValids2 (mergeItemSyncResponseIgnoreProblems @Int)
  describe "processServerItemSync" $ do
    it "produces valid responses and stores" $ producesValidsOnValids2 (processServerItemSync @Int)
    it "makes no changes if the sync request reflects the state of the empty server" $ do
      let store1 = ServerEmpty
          req = ItemSyncRequestPoll
      let (resp, store2) = processServerItemSync @Int store1 req
      store2 `shouldBe` store1
      resp `shouldBe` ItemSyncResponseInSyncEmpty
    it "makes no changes if the sync request reflects the state of the full server" $
      forAllValid $ \i ->
        forAllValid $ \st -> do
          let store1 = ServerFull $ Timed i st
              req = ItemSyncRequestKnown st
          let (resp, store2) = processServerItemSync @Int store1 req
          store2 `shouldBe` store1
          resp `shouldBe` ItemSyncResponseInSyncFull
    describe "Client changes" $ do
      it "adds the item that the client tells the server to add" $
        forAllValid $ \i -> do
          let store1 = ServerEmpty
              req = ItemSyncRequestNew i
          let (resp, store2) = processServerItemSync @Int store1 req
          let time = initialServerTime
          store2 `shouldBe` ServerFull (Timed i time)
          resp `shouldBe` ItemSyncResponseClientAdded time
      it "changes the item that the client tells the server to change" $
        forAllValid $ \i ->
          forAllValid $ \j ->
            forAllValid $ \st -> do
              let store1 = ServerFull (Timed i st)
                  req = ItemSyncRequestKnownButChanged (Timed j st)
              let (resp, store2) = processServerItemSync @Int store1 req
              let time = incrementServerTime st
              store2 `shouldBe` ServerFull (Timed j time)
              resp `shouldBe` ItemSyncResponseClientChanged time
      it "deletes the item that the client tells the server to delete" $
        forAllValid $ \i ->
          forAllValid $ \st -> do
            let store1 = ServerFull (Timed i st)
                req = ItemSyncRequestDeletedLocally st
            let (resp, store2) = processServerItemSync @Int store1 req
            store2 `shouldBe` ServerEmpty
            resp `shouldBe` ItemSyncResponseClientDeleted
    describe "Server changes" $ do
      it "tells the client that there is a new item at the server side" $ do
        forAllValid $ \i ->
          forAllValid $ \st -> do
            let store1 = ServerFull (Timed i st)
                req = ItemSyncRequestPoll
            let (resp, store2) = processServerItemSync @Int store1 req
            store2 `shouldBe` store1
            resp `shouldBe` ItemSyncResponseServerAdded (Timed i st)
      it "tells the client that there is a modified item at the server side" $ do
        forAllValid $ \i ->
          forAllSubsequent $ \(st, st') -> do
            let store1 = ServerFull (Timed i st')
                req = ItemSyncRequestKnown st
            let (resp, store2) = processServerItemSync @Int store1 req
            store2 `shouldBe` store1
            resp `shouldBe` ItemSyncResponseServerChanged (Timed i st')
      it "tells the client that there is a deleted item at the server side" $ do
        forAllValid $ \st -> do
          let store1 = ServerEmpty
              req = ItemSyncRequestKnown st
          let (resp, store2) = processServerItemSync @Int store1 req
          store2 `shouldBe` store1
          resp `shouldBe` ItemSyncResponseServerDeleted
    describe "Conflicts" $ do
      it "notices a conflict if the client and server are trying to sync different items" $
        forAllValid $ \i ->
          forAllValid $ \j ->
            forAllSubsequent $ \(st, st') -> do
              let store1 = ServerFull (Timed i st')
                  req = ItemSyncRequestKnownButChanged (Timed j st)
              let (resp, store2) = processServerItemSync @Int store1 req
              store2 `shouldBe` store1
              resp `shouldBe` ItemSyncResponseConflict i
      it
        "notices a server-deleted-conflict if the client has a deleted item and server has a modified item" $
        forAllValid $ \i ->
          forAllSubsequent $ \(st, st') -> do
            let store1 = ServerFull (Timed i st')
                req = ItemSyncRequestDeletedLocally st
            let (resp, store2) = processServerItemSync @Int store1 req
            store2 `shouldBe` store1
            resp `shouldBe` ItemSyncResponseConflictClientDeleted i
      it
        "notices a server-deleted-conflict if the client has a modified item and server has no item" $
        forAllValid $ \i ->
          forAllValid $ \st -> do
            let store1 = ServerEmpty
                req = ItemSyncRequestKnownButChanged (Timed i st)
            let (resp, store2) = processServerItemSync @Int store1 req
            store2 `shouldBe` store1
            resp `shouldBe` ItemSyncResponseConflictServerDeleted
  describe "syncing" $ do
    it "it always possible to add an item from scratch" $
      forAllValid $ \i -> do
        let cstore1 = ClientAdded (i :: Int)
        let sstore1 = ServerEmpty
        let req1 = makeItemSyncRequest cstore1
            (resp1, sstore2) = processServerItemSync sstore1 req1
            cstore2 = mergeItemSyncResponseIgnoreProblems cstore1 resp1
        let time = initialServerTime
        resp1 `shouldBe` ItemSyncResponseClientAdded time
        sstore2 `shouldBe` ServerFull (Timed i time)
        cstore2 `shouldBe` ClientItemSynced (Timed i time)
    it "succesfully syncs an addition across to a second client" $
      forAllValid $ \i -> do
        let cAstore1 = ClientAdded i
        -- Client B is empty
        let cBstore1 = ClientEmpty
        -- The server is empty
        let sstore1 = ServerEmpty
        -- Client A makes sync request 1
        let req1 = makeItemSyncRequest cAstore1
        -- The server processes sync request 1
        let (resp1, sstore2) = processServerItemSync @Int sstore1 req1
        let time = initialServerTime
        resp1 `shouldBe` ItemSyncResponseClientAdded time
        sstore2 `shouldBe` ServerFull (Timed i time)
        -- Client A merges the response
        let cAstore2 = mergeItemSyncResponseIgnoreProblems cAstore1 resp1
        cAstore2 `shouldBe` ClientItemSynced (Timed i time)
        -- Client B makes sync request 2
        let req2 = makeItemSyncRequest cBstore1
        -- The server processes sync request 2
        let (resp2, sstore3) = processServerItemSync sstore2 req2
        resp2 `shouldBe` ItemSyncResponseServerAdded (Timed i time)
        sstore3 `shouldBe` ServerFull (Timed i time)
        -- Client B merges the response
        let cBstore2 = mergeItemSyncResponseIgnoreProblems cBstore1 resp2
        cBstore2 `shouldBe` ClientItemSynced (Timed i time)
        -- Client A and Client B now have the same store
        cAstore2 `shouldBe` cBstore2
    it "succesfully syncs a modification across to a second client" $
      forAllValid $ \time1 ->
        forAllValid $ \i ->
          forAllValid $ \j -> do
            let cAstore1 = ClientItemSynced (Timed i time1)
            -- Client B had synced that same item, but has since modified it
            let cBstore1 = ClientItemSyncedButChanged (Timed j time1)
            -- The server is has the item that both clients had before
            let sstore1 = ServerFull (Timed i time1)
            -- Client B makes sync request 1
            let req1 = makeItemSyncRequest cBstore1
            -- The server processes sync request 1
            let (resp1, sstore2) = processServerItemSync @Int sstore1 req1
            let time2 = incrementServerTime time1
            resp1 `shouldBe` ItemSyncResponseClientChanged time2
            sstore2 `shouldBe` ServerFull (Timed j time2)
            -- Client B merges the response
            let cBstore2 = mergeItemSyncResponseIgnoreProblems cBstore1 resp1
            cBstore2 `shouldBe` ClientItemSynced (Timed j time2)
            -- Client A makes sync request 2
            let req2 = makeItemSyncRequest cAstore1
            -- The server processes sync request 2
            let (resp2, sstore3) = processServerItemSync sstore2 req2
            resp2 `shouldBe` ItemSyncResponseServerChanged (Timed j time2)
            sstore3 `shouldBe` ServerFull (Timed j time2)
            -- Client A merges the response
            let cAstore2 = mergeItemSyncResponseIgnoreProblems cAstore1 resp2
            cAstore2 `shouldBe` ClientItemSynced (Timed j time2)
            -- Client A and Client B now have the same store
            cAstore2 `shouldBe` cBstore2
    it "succesfully syncs a deletion across to a second client" $
      forAllValid $ \time1 ->
        forAllValid $ \i -> do
          let cAstore1 = ClientItemSynced (Timed i time1)
          -- Client B had synced that same item, but has since deleted it
          let cBstore1 = ClientDeleted time1
          -- The server still has the undeleted item
          let sstore1 = ServerFull (Timed i time1)
          -- Client B makes sync request 1
          let req1 = makeItemSyncRequest cBstore1
          -- The server processes sync request 1
          let (resp1, sstore2) = processServerItemSync @Int sstore1 req1
          resp1 `shouldBe` ItemSyncResponseClientDeleted
          sstore2 `shouldBe` ServerEmpty
          -- Client B merges the response
          let cBstore2 = mergeItemSyncResponseIgnoreProblems cBstore1 resp1
          cBstore2 `shouldBe` ClientEmpty
          -- Client A makes sync request 2
          let req2 = makeItemSyncRequest cAstore1
          -- The server processes sync request 2
          let (resp2, sstore3) = processServerItemSync sstore2 req2
          resp2 `shouldBe` ItemSyncResponseServerDeleted
          sstore3 `shouldBe` ServerEmpty
          -- Client A merges the response
          let cAstore2 = mergeItemSyncResponseIgnoreProblems cAstore1 resp2
          cAstore2 `shouldBe` ClientEmpty
          -- Client A and Client B now have the same store
          cAstore2 `shouldBe` cBstore2
    it "does not run into a conflict if two clients both try to sync a deletion" $
      forAllValid $ \time1 ->
        forAllValid $ \i -> do
          let cAstore1 = ClientDeleted time1
          -- Both client a and client b delete an item.
          let cBstore1 = ClientDeleted time1
          -- The server still has the undeleted item
          let sstore1 = ServerFull (Timed i time1)
          -- Client A makes sync request 1
          let req1 = makeItemSyncRequest cAstore1
          -- The server processes sync request 1
          let (resp1, sstore2) = processServerItemSync @Int sstore1 req1
          resp1 `shouldBe` ItemSyncResponseClientDeleted
          sstore2 `shouldBe` ServerEmpty
          -- Client A merges the response
          let cAstore2 = mergeItemSyncResponseIgnoreProblems cAstore1 resp1
          cAstore2 `shouldBe` ClientEmpty
          -- Client B makes sync request 2
          let req2 = makeItemSyncRequest cBstore1
          -- The server processes sync request 2
          let (resp2, sstore3) = processServerItemSync sstore2 req2
          resp2 `shouldBe` ItemSyncResponseClientDeleted
          sstore3 `shouldBe` ServerEmpty
          -- Client B merges the response
          let cBstore2 = mergeItemSyncResponseIgnoreProblems cBstore1 resp2
          cBstore2 `shouldBe` ClientEmpty
          -- Client A and Client B now have the same store
          cAstore2 `shouldBe` cBstore2
    it "is idempotent with one client" $
      forAllValid $ \cstore1 ->
        forAllValid $ \sstore1 -> do
          let req1 = makeItemSyncRequest (cstore1 :: ClientItem Int)
              (resp1, sstore2) = processServerItemSync sstore1 req1
              cstore2 = mergeItemSyncResponseIgnoreProblems cstore1 resp1
              req2 = makeItemSyncRequest cstore2
              (resp2, sstore3) = processServerItemSync sstore2 req2
              cstore3 = mergeItemSyncResponseIgnoreProblems cstore2 resp2
          cstore2 `shouldBe` cstore3
          sstore2 `shouldBe` sstore3
