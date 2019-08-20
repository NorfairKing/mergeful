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
import qualified Data.Map as M
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
            sort (M.elems (M.map timedValue (clientStoreSyncedItems cstore2))) `shouldBe` sort items
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
