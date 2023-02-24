{-# LANGUAGE TypeApplications #-}

module Data.Mergeful.DirForestSpec (spec) where

import Data.GenValidity.Mergeful.DirForest ()
import Data.Mergeful.DirForest
import Data.Word
import Test.Syd hiding (Timed (..))
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  describe "ClientDirForest" $ do
    genValidSpec @(ClientDirForest Word8)
    jsonSpec @(ClientDirForest Word8)
  describe "ServerDirForest" $ do
    genValidSpec @(ServerDirForest Word8)
    jsonSpec @(ServerDirForest Word8)
  describe "DirForestSyncRequest" $ do
    genValidSpec @(DirForestSyncRequest Word8)
    jsonSpec @(DirForestSyncRequest Word8)
  describe "DirForestSyncResponse" $ do
    genValidSpec @(DirForestSyncResponse Word8)
    jsonSpec @(DirForestSyncResponse Word8)
  describe "makeDirForestSyncRequest" $
    it "produces valid requests" $
      producesValid (makeDirForestSyncRequest @Word8)
  describe "processServerSync" $
    it "produces valid server dir forests" $
      producesValid2 (processServerSync @Word8)
  describe "mergeDirForestSyncResponse" $
    it "produces valid client dir forests" $
      producesValid2 (mergeDirForestSyncResponse @Word8)
