{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()

import Data.GenValidity.Mergeful.Item ()

import Data.Mergeful

instance (GenUnchecked i, Ord i, GenUnchecked a) => GenUnchecked (ClientStore i a)

instance (GenValid i, Ord i, GenValid a) => GenValid (ClientStore i a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked i, Ord i, GenUnchecked a) => GenUnchecked (ServerStore i a)

instance (GenValid i, Ord i, GenValid a) => GenValid (ServerStore i a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenUnchecked i, Ord i, GenUnchecked a) => GenUnchecked (SyncRequest i a)

instance (GenValid i, Ord i, GenValid a) => GenValid (SyncRequest i a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked i, GenUnchecked a) => GenUnchecked (SyncResponse i a)

instance (GenValid i, GenValid a) => GenValid (SyncResponse i a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
