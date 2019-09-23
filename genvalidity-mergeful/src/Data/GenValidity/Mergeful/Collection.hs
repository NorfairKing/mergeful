{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful.Collection where

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

instance GenUnchecked ClientId

instance GenValid ClientId

instance (GenUnchecked i, Ord i, GenUnchecked a) => GenUnchecked (ClientStore i a)

instance (GenValid i, Show i, Ord i, GenValid a) => GenValid (ClientStore i a) where
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

instance (GenValid i, Show i, Ord i, GenValid a) => GenValid (ServerStore i a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenUnchecked i, Ord i, GenUnchecked a) => GenUnchecked (SyncRequest i a)

instance (GenValid i, Show i, Ord i, GenValid a) => GenValid (SyncRequest i a) where
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

instance (GenValid i, Show i, Ord i, GenValid a) => GenValid (SyncResponse i a) where
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
      (s15, s16) <- splitSet s07
      syncResponseClientAdded <-
        do m <- genValid :: Gen (Map ClientId ())
           if S.null s08
             then pure M.empty
             else forM m $ \() -> (,) <$> elements (S.toList s08) <*> genValid
      syncResponseClientChanged <- mapWithIds s09
      let syncResponseClientDeleted = s10
      syncResponseServerAdded <- mapWithIds s11
      syncResponseServerChanged <- mapWithIds s12
      let syncResponseServerDeleted = s13
      syncResponseConflicts <- mapWithIds s14
      syncResponseConflictsClientDeleted <- mapWithIds s15
      let syncResponseConflictsServerDeleted = s16
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
