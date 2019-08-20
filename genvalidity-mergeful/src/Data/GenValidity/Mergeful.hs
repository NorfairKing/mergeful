{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Data.Traversable

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()

import Test.QuickCheck

import Data.GenValidity.Mergeful.Item ()

import Data.Mergeful

instance (GenUnchecked i, Ord i, GenUnchecked a) => GenUnchecked (ClientStore i a)

instance (GenValid i, Ord i, GenValid a) => GenValid (ClientStore i a) where
  genValid =
    (`suchThat` isValid) $ do
      identifiers <- scale (* 3) genValid
      (s1, s2) <- splitSet identifiers
      (s3, s4) <- splitSet s1
      clientStoreAddedItems <- genValid
      clientStoreSyncedItems <- mapWithIds s2
      clientStoreSyncedButChangedItems <- mapWithIds s3
      clientStoreDeletedItems <- mapWithIds s4
      pure ClientStore {..}
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked i, Ord i, GenUnchecked a) => GenUnchecked (ServerStore i a)

instance (GenValid i, Ord i, GenValid a) => GenValid (ServerStore i a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenUnchecked i, Ord i, GenUnchecked a) => GenUnchecked (SyncRequest i a)

instance (GenValid i, Ord i, GenValid a) => GenValid (SyncRequest i a) where
  genValid =
    (`suchThat` isValid) $ do
      identifiers <- scale (* 3) genValid
      (s1, s2) <- splitSet identifiers
      (s3, s4) <- splitSet s1
      syncRequestNewItems <- genValid
      syncRequestKnownItems <- mapWithIds s2
      syncRequestKnownButChangedItems <- mapWithIds s3
      syncRequestDeletedItems <- mapWithIds s4
      pure SyncRequest {..}
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked i, Ord i, GenUnchecked a) => GenUnchecked (SyncResponse i a)

instance (GenValid i, Ord i, GenValid a) => GenValid (SyncResponse i a) where
  genValid =
    (`suchThat` isValid) $ do
      identifiers <- scale (* 10) genValid
      (s01, s02) <- splitSet identifiers
      (s03, s04) <- splitSet s01
      (s05, s06) <- splitSet s02
      (s07, s08) <- splitSet s03
      (s09, s10) <- splitSet s04
      (s11, s12) <- splitSet s05
      (s13, s14) <- splitSet s06
      syncResponseAddedItems <-
        do m <- (genValid :: Gen (Map Int ()))
           if S.null s07
             then pure M.empty
             else forM m $ \() -> (,) <$> elements (S.toList s07) <*> genValid
      syncResponseNewRemoteItems <- mapWithIds s08
      syncResponseModifiedByServerItems <- mapWithIds s09
      syncResponseModifiedByClientItems <- mapWithIds s10
      let syncResponseItemsToBeDeletedLocally = s11
      syncResponseConflicts <- mapWithIds s12
      syncResponseConflictsClientDeleted <- mapWithIds s13
      let syncResponseConflictsServerDeleted = s14
      pure SyncResponse {..}
  shrinkValid = shrinkValidStructurally

splitSet :: Ord i => Set i -> Gen (Set i, Set i)
splitSet s =
  if S.null s
    then pure (S.empty, S.empty)
    else do
      a <- elements $ S.toList s
      pure $ S.split a s

mapWithIds :: (Ord i, GenValid a) => Set i -> Gen (Map i a)
mapWithIds s = fmap M.fromList $ for (S.toList s) $ \i -> (,) i <$> genValid
