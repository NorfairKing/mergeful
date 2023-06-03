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
  let yamlSchemaSpec :: forall a. (Typeable a, GenValid a, HasCodec a) => FilePath -> Spec
      yamlSchemaSpec filePath = do
        it ("outputs the same schema as before for " <> nameOf @a) $
          pureGoldenTextFile
            ("test_resources/dirforest/" <> filePath <> ".txt")
            (renderChunksText With24BitColours $ schemaChunksViaCodec @a)
  describe "ClientDirForest" $ do
    genValidSpec @(ClientDirForest Word8)
    jsonSpec @(ClientDirForest Word8)
    yamlSchemaSpec @(ClientDirForest Word8) "client-dirforest"

  describe "ServerDirForest" $ do
    genValidSpec @(ServerDirForest Word8)
    jsonSpec @(ServerDirForest Word8)
    yamlSchemaSpec @(ClientDirForest Word8) "server-dirforest"

  describe "DirForestSyncRequest" $ do
    genValidSpec @(DirForestSyncRequest Word8)
    jsonSpec @(DirForestSyncRequest Word8)
    yamlSchemaSpec @(ClientDirForest Word8) "dirforest-request"

  describe "DirForestSyncResponse" $ do
    genValidSpec @(DirForestSyncResponse Word8)
    jsonSpec @(DirForestSyncResponse Word8)
    yamlSchemaSpec @(ClientDirForest Word8) "dirforest-response"

  describe "makeDirForestSyncRequest" $
    it "produces valid requests" $
      producesValid (makeDirForestSyncRequest @Word8)

  describe "processServerSync" $
    it "produces valid server dir forests" $
      producesValid2 (processServerSync @Word8)

  describe "mergeDirForestSyncResponse" $
    it "produces valid client dir forests" $
      producesValid2 (mergeDirForestSyncResponse @Word8)

  describe "One client" $ do
    pending "Can sync over an addition"
    pending "Can sync over a modification"
    pending "Can sync over a deletion"
  describe "Two clients" $ do
    pending "Can sync over an addition"
    pending "Can sync over a modification"
    pending "Can sync over a deletion"

emptyResponseSpec ::
  forall a.
  (Show a, Eq a, Ord a, GenValid a) =>
  ItemMergeStrategy a ->
  Spec
emptyResponseSpec strat = do
  let mergeFunc = mergeSyncResponseUsingStrategy strat
  it "is returns an empty response on the second sync with no modifications" $
    forAllValid $ \cstore1 ->
      forAllValid $ \sstore1 ->
        evalDM $ do
          let req1 = makeSyncRequest @ClientId @UUID cstore1
          (resp1, sstore2) <- processServerSync genD sstore1 req1
          let cstore2 = mergeFunc cstore1 resp1
              req2 = makeSyncRequest cstore2
          (resp2, _) <- processServerSync genD sstore2 req2
          lift $ resp2 `shouldBe` emptySyncResponse
