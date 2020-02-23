{-# LANGUAGE TypeApplications #-}

module Data.Mergeful.DirForestSpec where

import Test.Hspec
import Test.Validity

import Data.GenValidity.Mergeful.DirForest
import Data.Mergeful.DirForest

spec :: Spec
spec = do
  genValidSpec @(ClientForest Int)
  genValidSpec @(ServerForest Int)
  genValidSpec @(ForestSyncRequest Int)
  genValidSpec @(ForestSyncResponse Int)
  describe "initialClientForest" $ it "is valid" $ shouldBeValid (initialClientForest @Int)
  describe "initialServerForest" $ it "is valid" $ shouldBeValid (initialServerForest @Int)
  describe "makeForestSyncRequest" $
    it "produces valid sync requests" $ producesValidsOnValids (makeForestSyncRequest @Int)
  describe "mergeForestSyncResponseFromClient" $
    it "produces valid client forests" $
    producesValidsOnValids2 (mergeForestSyncResponseFromClient @Int)
  describe "mergeForestSyncResponseFromServer" $
    it "produces valid client forests" $
    producesValidsOnValids2 (mergeForestSyncResponseFromServer @Int)
  describe "mergeForestSyncResponseUsingStrategy" $
    it "produces valid client forests" $
    producesValidsOnValids2 $
    mergeForestSyncResponseUsingStrategy
      @Int
      ItemMergeStrategy
        { itemMergeStrategyMergeChangeConflict = \clientItem serverItem -> max clientItem serverItem
        , itemMergeStrategyMergeClientDeletedConflict = \serverItem -> Just serverItem
        , itemMergeStrategyMergeServerDeletedConflict = \_ -> Nothing
        }
  describe "processServerForestSync" $
    it "produces valid responses" $ producesValidsOnValids2 (processServerForestSync @Int)
