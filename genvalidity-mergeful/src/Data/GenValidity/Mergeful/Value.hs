{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful.Value where

import Data.GenValidity

import Data.Mergeful.Value

import Data.GenValidity.Mergeful.Timed ()

instance GenUnchecked a => GenUnchecked (ClientValue a)

instance GenValid a => GenValid (ClientValue a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (ServerValue a)

instance GenValid a => GenValid (ServerValue a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (ValueSyncRequest a)

instance GenValid a => GenValid (ValueSyncRequest a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (ValueSyncResponse a)

instance GenValid a => GenValid (ValueSyncResponse a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
