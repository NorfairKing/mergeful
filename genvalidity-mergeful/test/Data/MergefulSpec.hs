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

import Data.GenValidity.Mergeful ()
import Data.GenValidity.UUID.Typed ()
import Data.Mergeful
import Data.UUID.Typed

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

forAllNext func = forAllValid $ \st -> func (st, incrementServerTime st)

forAllSubsequent func =
  forAllValid $ \st -> forAll (genValid `suchThat` (> st)) $ \st' -> func (st, st')

spec :: Spec
spec = do
  genValidSpec @ClientStore
  genValidSpec @ServerTime
  genValidSpec @ServerStore
  genValidSpec @ServerState
  genValidSpec @SyncRequest
  genValidSpec @SyncResponse
  describe "initialServerTime" $ it "is valid" $ shouldBeValid initialServerTime
  describe "makeSyncRequest" $ it "produces valid requests" $ producesValidsOnValids makeSyncRequest
  describe "mergeSyncResponse" $
    it "produces valid client stores" $ producesValidsOnValids2 mergeSyncResponse
  describe "processServerSync" $
    it "produces valid responses and stores" $ producesValidsOnValids2 processServerSync
  describe "syncing" $ do
    it "it always possible to add an item from scratch" $
      forAllValid $ \i -> do
        let cstore1 = ClientAdded i
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
          let req1 = makeSyncRequest cstore1
              (resp1, sstore2) = processServerSync sstore1 req1
              cstore2 = mergeSyncResponse cstore1 resp1
              req2 = makeSyncRequest cstore2
              (resp2, sstore3) = processServerSync sstore2 req2
              cstore3 = mergeSyncResponse cstore2 resp2
          cstore2 `shouldBe` cstore3
