{-# LANGUAGE TypeApplications #-}

module Data.Mergeful.ValueSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

import Data.Mergeful.Timed
import Data.Mergeful.Value

import Data.GenValidity.Mergeful.Value ()

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

forAllSubsequent :: Testable prop => ((ServerTime, ServerTime) -> prop) -> Property
forAllSubsequent func =
  forAllValid $ \st ->
    forAllShrink (genValid `suchThat` (> st)) (filter (> st) . shrinkValid) $ \st' -> func (st, st')

spec :: Spec
spec = do
  genValidSpec @(ClientValue Int)
  jsonSpecOnValid @(ClientValue Int)
  genValidSpec @(ServerValue Int)
  jsonSpecOnValid @(ServerValue Int)
  genValidSpec @(ValueSyncRequest Int)
  jsonSpecOnValid @(ValueSyncRequest Int)
  genValidSpec @(ValueSyncResponse Int)
  jsonSpecOnValid @(ValueSyncResponse Int)
  describe "makeValueSyncRequest" $
    it "produces valid requests" $ producesValidsOnValids (makeValueSyncRequest @Int)
  describe "mergeValueSyncResponseRaw" $
    it "produces valid client stores" $ producesValidsOnValids2 (mergeValueSyncResponseRaw @Int)
  describe "mergeValueSyncResponseIgnoreProblems" $
    it "produces valid client stores" $
    producesValidsOnValids2 (mergeValueSyncResponseIgnoreProblems @Int)
  describe "processServerValueSync" $ do
    it "produces valid responses and stores" $ producesValidsOnValids2 (processServerValueSync @Int)
    it "makes no changes if the sync request reflects the state of the server" $
      forAllValid $ \i ->
        forAllValid $ \st -> do
          let store1 = ServerValue $ Timed i st
              req = ValueSyncRequestKnown st
          let (resp, store2) = processServerValueSync @Int store1 req
          store2 `shouldBe` store1
          resp `shouldBe` ValueSyncResponseInSync
    describe "Client changes" $ do
      it "changes the item that the client tells the server to change" $
        forAllValid $ \i ->
          forAllValid $ \j ->
            forAllValid $ \st -> do
              let store1 = ServerValue (Timed i st)
                  req = ValueSyncRequestKnownButChanged (Timed j st)
              let (resp, store2) = processServerValueSync @Int store1 req
              let time = incrementServerTime st
              store2 `shouldBe` ServerValue (Timed j time)
              resp `shouldBe` ValueSyncResponseClientChanged time
    describe "Server changes" $ do
      it "tells the client that there is a modified item at the server side" $ do
        forAllValid $ \i ->
          forAllSubsequent $ \(st, st') -> do
            let store1 = ServerValue (Timed i st')
                req = ValueSyncRequestKnown st
            let (resp, store2) = processServerValueSync @Int store1 req
            store2 `shouldBe` store1
            resp `shouldBe` ValueSyncResponseServerChanged (Timed i st')
    describe "Conflicts" $ do
      it "notices a conflict if the client and server are trying to sync different items" $
        forAllValid $ \i ->
          forAllValid $ \j ->
            forAllSubsequent $ \(st, st') -> do
              let store1 = ServerValue (Timed i st')
                  req = ValueSyncRequestKnownButChanged (Timed j st)
              let (resp, store2) = processServerValueSync @Int store1 req
              store2 `shouldBe` store1
              resp `shouldBe` ValueSyncResponseConflict i
  describe "syncing" $ do
    it "succesfully syncs a modification across to a second client" $
      forAllValid $ \time1 ->
        forAllValid $ \i ->
          forAllValid $ \j -> do
            let cAstore1 = ClientValue (Timed i time1) NotChanged
            -- Client B had synced that same item, but has since modified it
            let cBstore1 = ClientValue (Timed j time1) Changed
            -- The server is has the item that both clients had before
            let sstore1 = ServerValue (Timed i time1)
            -- Client B makes sync request 1
            let req1 = makeValueSyncRequest cBstore1
            -- The server processes sync request 1
            let (resp1, sstore2) = processServerValueSync @Int sstore1 req1
            let time2 = incrementServerTime time1
            resp1 `shouldBe` ValueSyncResponseClientChanged time2
            sstore2 `shouldBe` ServerValue (Timed j time2)
            -- Client B merges the response
            let cBstore2 = mergeValueSyncResponseIgnoreProblems cBstore1 resp1
            cBstore2 `shouldBe` ClientValue (Timed j time2) NotChanged
            -- Client A makes sync request 2
            let req2 = makeValueSyncRequest cAstore1
            -- The server processes sync request 2
            let (resp2, sstore3) = processServerValueSync sstore2 req2
            resp2 `shouldBe` ValueSyncResponseServerChanged (Timed j time2)
            sstore3 `shouldBe` ServerValue (Timed j time2)
            -- Client A merges the response
            let cAstore2 = mergeValueSyncResponseIgnoreProblems cAstore1 resp2
            cAstore2 `shouldBe` ClientValue (Timed j time2) NotChanged
            -- Client A and Client B now have the same store
            cAstore2 `shouldBe` cBstore2
    it "is idempotent with one client" $
      forAllValid $ \cstore1 ->
        forAllValid $ \sstore1 -> do
          let req1 = makeValueSyncRequest (cstore1 :: ClientValue Int)
              (resp1, sstore2) = processServerValueSync sstore1 req1
              cstore2 = mergeValueSyncResponseIgnoreProblems cstore1 resp1
              req2 = makeValueSyncRequest cstore2
              (resp2, sstore3) = processServerValueSync sstore2 req2
              cstore3 = mergeValueSyncResponseIgnoreProblems cstore2 resp2
          cstore2 `shouldBe` cstore3
          sstore2 `shouldBe` sstore3
