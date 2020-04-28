{-# LANGUAGE TypeApplications #-}

module Data.Mergeful.DirForestSpec where

import Data.GenValidity.Mergeful.DirForest
import Data.Mergeful.DirForest
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @(ClientForest Int)
  genValidSpec @(ServerForest Int)
  genValidSpec @(ForestSyncRequest Int)
  genValidSpec @(ForestSyncResponse Int)
  describe "initialClientForest" $ it "is valid" $ shouldBeValid (initialClientForest @Int)
  describe "initialServerForest" $ it "is valid" $ shouldBeValid (initialServerForest @Int)
  describe "makeForestSyncRequest"
    $ it "produces valid sync requests"
    $ producesValidsOnValids (makeForestSyncRequest @Int)
  describe "mergeForestSyncResponseFromClient"
    $ it "produces valid client forests"
    $ producesValidsOnValids2 (mergeForestSyncResponseFromClient @Int)
  describe "mergeForestSyncResponseFromServer"
    $ it "produces valid client forests"
    $ producesValidsOnValids2 (mergeForestSyncResponseFromServer @Int)
  describe "mergeForestSyncResponseUsingStrategy"
    $ it "produces valid client forests"
    $ producesValidsOnValids2
    $ mergeForestSyncResponseUsingStrategy
      @Int
      ItemMergeStrategy
        { itemMergeStrategyMergeChangeConflict = \clientItem serverItem -> max clientItem serverItem,
          itemMergeStrategyMergeClientDeletedConflict = \serverItem -> Just serverItem,
          itemMergeStrategyMergeServerDeletedConflict = \_ -> Nothing
        }
  describe "processServerForestSync"
    $ it "produces valid responses"
    $ producesValidsOnValids2 (processServerForestSync @Int)
  let pend = it "nothing" pending
      mergeFunc = mergeForestSyncResponseFromServer
  describe "Single client" $ do
    it "is idempotent" $ forAllValid $ \cf1 -> forAllValid $ \sf1 -> do
      let req1 = makeForestSyncRequest (cf1 :: ClientForest Int)
          (resp1, sf2) = processServerForestSync sf1 req1
          cf2 = mergeFunc cf1 resp1
      let req2 = makeForestSyncRequest cf2
          (resp2, sf3) = processServerForestSync sf2 req2
          cf3 = mergeFunc cf2 resp2
      cf3 `shouldBe` cf2
    describe "Single item" $ do
      describe "Addition" pend
      describe "Modification" pend
      describe "Deletion" pend
      describe "Conflict" pend
    describe
      "Multi-item item"
      $ do
        describe "Addition" pend
        describe "Modification" pend
        describe "Deletion" pend
        describe "Conflict" pend
  describe "Multiple clients" $ do
    describe "Single item" $ do
      describe "Addition" pend
      describe "Modification" pend
      describe "Deletion" pend
      describe "Conflict" pend
    describe
      "Multi-item item"
      $ do
        describe "Addition" pend
        describe "Modification" pend
        describe "Deletion" pend
        describe "Conflict" pend
