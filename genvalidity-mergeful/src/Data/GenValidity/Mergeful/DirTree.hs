{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful.DirTree where

import Data.GenValidity
import Data.GenValidity.Path ()

import Data.Mergeful.DirTree

instance GenUnchecked a => GenUnchecked (DirTree a)

instance (GenUnchecked a, GenInvalid a) => GenInvalid (DirTree a)

instance (GenUnchecked a, GenValid a) => GenValid (DirTree a)
