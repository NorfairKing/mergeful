{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful.DirForest where

import qualified Data.Map as M
import Path
import qualified System.FilePath as FP

import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Path ()

import Data.GenValidity.Mergeful.DirTree.Type ()
import Data.GenValidity.Mergeful.Item ()

import Data.Mergeful.DirForest

instance (Ord a, GenUnchecked a) => GenUnchecked (ClientForest a)

instance (Ord a, GenUnchecked a, GenInvalid a) => GenInvalid (ClientForest a)

instance (Ord a, GenValid a) => GenValid (ClientForest a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (Ord a, GenUnchecked a) => GenUnchecked (ForestSyncRequest a)

instance (Ord a, GenUnchecked a, GenInvalid a) => GenInvalid (ForestSyncRequest a)

instance (Ord a, GenValid a) => GenValid (ForestSyncRequest a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (Ord a, GenUnchecked a) => GenUnchecked (ForestSyncResponse a)

instance (Ord a, GenUnchecked a, GenInvalid a) => GenInvalid (ForestSyncResponse a)

instance (Ord a, GenValid a) => GenValid (ForestSyncResponse a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
