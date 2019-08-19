{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.MergefulSpec
  ( spec
  ) where

import Data.Int (Int)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time
import qualified Data.UUID.Typed as Typed
import GHC.Generics (Generic)
import System.Random

import Control.Monad.State

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

import Data.GenValidity.Mergeful
import Data.GenValidity.UUID.Typed ()
import Data.Mergeful
import Data.UUID.Typed

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- forAllNext func = forAllValid $ \st -> func (st, incrementServerTime st)
forAllSubsequent func =
  forAllValid $ \st -> forAll (genValid `suchThat` (> st)) $ \st' -> func (st, st')

spec :: Spec
spec = do
  genValidSpec @ServerTime
  genValidSpec @(ClientStore Int)
  genValidSpec @(ServerStore Int)
  genValidSpec @(ServerState Int)
  genValidSpec @(SyncRequest Int)
  genValidSpec @(SyncResponse Int)
  describe "initialServerTime" $ it "is valid" $ shouldBeValid initialServerTime
  describe "makeSyncRequest" $
    it "produces valid requests" $ producesValidsOnValids (makeSyncRequest @Int)
  describe "mergeSyncResponse" $
    it "produces valid client stores" $ producesValidsOnValids2 (mergeSyncResponse @Int)
  describe "processServerSync" $ do
    it "produces valid responses and stores" $ producesValidsOnValids2 (processServerSync @Int)
    it "makes no changes if the sync request reflects the state of the empty server" $
      forAllValid $ \st -> do
        let store1 = ServerState st $ ServerEmpty st
            req = SyncRequestPoll
        let (resp, store2) = processServerSync @Int store1 req
        store2 `shouldBe` store1
        resp `shouldBe` SyncResponseInSyncEmpty
    it "makes no changes if the sync request reflects the state of the full server" $
      forAllValid $ \i ->
        forAllValid $ \st -> do
          let store1 = ServerState st $ ServerFull i st
              req = SyncRequestKnown st
          let (resp, store2) = processServerSync @Int store1 req
          store2 `shouldBe` store1
          resp `shouldBe` SyncResponseInSyncFull
    describe "Client changes" $ do
      it "adds the item that the client tells the server to add" $
        forAllValid $ \i ->
          forAllValid $ \st -> do
            let store1 = ServerState st $ ServerEmpty st
                req = SyncRequestNew i
            let (resp, store2) = processServerSync @Int store1 req
            let time = incrementServerTime st
            store2 `shouldBe` ServerState time (ServerFull i time)
            resp `shouldBe` SyncResponseSuccesfullyAdded time
      it "changes the item that the client tells the server to change" $
        forAllValid $ \i ->
          forAllValid $ \j ->
            forAllValid $ \st -> do
              let store1 = ServerState st $ ServerFull i st
                  req = SyncRequestKnownButChanged j st
              let (resp, store2) = processServerSync @Int store1 req
              let time = incrementServerTime st
              store2 `shouldBe` ServerState time (ServerFull j time)
              resp `shouldBe` SyncResponseSuccesfullyChanged time
      it "deletes the item that the client tells the server to delete" $
        forAllValid $ \i ->
          forAllValid $ \st -> do
            let store1 = ServerState st $ ServerFull i st
                req = SyncRequestDeletedLocally st
            let (resp, store2) = processServerSync @Int store1 req
            let time = incrementServerTime st
            store2 `shouldBe` ServerState time (ServerEmpty time)
            resp `shouldBe` SyncResponseSuccesfullyDeleted
    describe "Server changes" $ do
      it "tells the client that there is a new item at the server side" $ do
        forAllValid $ \i ->
          forAllValid $ \st -> do
            let store1 = ServerState st $ ServerFull i st
                req = SyncRequestPoll
            let (resp, store2) = processServerSync @Int store1 req
            store2 `shouldBe` store1
            resp `shouldBe` SyncResponseNewAtServer i st
      it "tells the client that there is a modified item at the server side" $ do
        forAllValid $ \i ->
          forAllSubsequent $ \(st, st') -> do
            let store1 = ServerState st' $ ServerFull i st'
                req = SyncRequestKnown st
            let (resp, store2) = processServerSync @Int store1 req
            store2 `shouldBe` store1
            resp `shouldBe` SyncResponseModifiedAtServer i st'
      it "tells the client that there is a deleted item at the server side" $ do
        forAllSubsequent $ \(st, st') -> do
          let store1 = ServerState st' $ ServerEmpty st'
              req = SyncRequestKnown st
          let (resp, store2) = processServerSync @Int store1 req
          store2 `shouldBe` store1
          resp `shouldBe` SyncResponseDeletedAtServer
    describe "Conflicts" $ do
      it "notices a conflict if the client and server are trying to sync different items" $
        forAllValid $ \i ->
          forAllValid $ \j ->
            forAllSubsequent $ \(st, st') -> do
              let store1 = ServerState st' $ ServerFull i st'
                  req = SyncRequestKnownButChanged j st
              let (resp, store2) = processServerSync @Int store1 req
              store2 `shouldBe` store1
              resp `shouldBe` SyncResponseConflict i
      it
        "notices a server-deleted-conflict if the client has a deleted item and server has a modified item" $
        forAllValid $ \i ->
          forAllSubsequent $ \(st, st') -> do
            let store1 = ServerState st' $ ServerFull i st'
                req = SyncRequestDeletedLocally st
            let (resp, store2) = processServerSync @Int store1 req
            store2 `shouldBe` store1
            resp `shouldBe` SyncResponseConflictClientDeleted i
      it
        "notices a server-deleted-conflict if the client has a modified item and server has no item" $
        forAllValid $ \i ->
          forAllSubsequent $ \(st, st') -> do
            let store1 = ServerState st' $ ServerEmpty st'
                req = SyncRequestKnownButChanged i st
            let (resp, store2) = processServerSync @Int store1 req
            store2 `shouldBe` store1
            resp `shouldBe` SyncResponseConflictServerDeleted
    describe "Desyncs" $ do
      it "notices a desync if the client is somehow ahead of the server" $
        forAllSubsequent $ \(st, st') ->
          forAll (stateAt st) $ \store1 ->
            forAll (reqAt st') $ \req -> do
              let (resp, store2) = processServerSync @Int store1 req
              store2 `shouldBe` store1
              case resp of
                SyncResponseDesynchronised _st _ -> _st `shouldBe` st
  describe "syncing" $ do
    it "it always possible to add an item from scratch" $
      forAllValid $ \i -> do
        let cstore1 = ClientAdded (i :: Int)
        let sstore1 = initialServerState
        let req1 = makeSyncRequest cstore1
            (resp1, sstore2) = processServerSync sstore1 req1
            cstore2 = mergeSyncResponse cstore1 resp1
        let time = incrementServerTime initialServerTime -- A change occurred, so we need to increment.
        resp1 `shouldBe` SyncResponseSuccesfullyAdded time
        sstore2 `shouldBe` ServerState time (ServerFull i time)
        cstore2 `shouldBe` ClientSynced i time
    it "is idempotent with one client" $
      forAllValid $ \cstore1 ->
        forAllValid $ \sstore1 -> do
          let req1 = makeSyncRequest (cstore1 :: ClientStore Int)
              (resp1, sstore2) = processServerSync sstore1 req1
              cstore2 = mergeSyncResponse cstore1 resp1
              req2 = makeSyncRequest cstore2
              (resp2, sstore3) = processServerSync sstore2 req2
              cstore3 = mergeSyncResponse cstore2 resp2
          cstore2 `shouldBe` cstore3
