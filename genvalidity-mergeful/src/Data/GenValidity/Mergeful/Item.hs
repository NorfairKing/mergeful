{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful.Item where

import Data.GenValidity
import Data.GenValidity.Mergeful.Timed ()
import Data.Mergeful.Item

instance GenUnchecked a => GenUnchecked (ItemMergeResult a)

instance GenValid a => GenValid (ItemMergeResult a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (ClientItem a)

instance GenValid a => GenValid (ClientItem a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (ServerItem a)

instance GenValid a => GenValid (ServerItem a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (ItemSyncRequest a)

instance GenValid a => GenValid (ItemSyncRequest a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (ItemSyncResponse a)

instance GenValid a => GenValid (ItemSyncResponse a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
