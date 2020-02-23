{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful.DirTree where

import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Path ()

import Data.Mergeful.DirTree

instance (Ord a, GenUnchecked a) => GenUnchecked (DirTree a)

instance (Ord a, GenUnchecked a, GenInvalid a) => GenInvalid (DirTree a)

instance (Ord a, GenUnchecked a, GenValid a) => GenValid (DirTree a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (Ord a, GenUnchecked a) => GenUnchecked (DirForest a)

instance (Ord a, GenUnchecked a, GenInvalid a) => GenInvalid (DirForest a)

instance (Ord a, GenUnchecked a, GenValid a) => GenValid (DirForest a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
