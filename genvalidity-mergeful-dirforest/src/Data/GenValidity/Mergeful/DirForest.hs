{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.GenValidity.Mergeful.DirForest where

import Data.GenValidity
import Data.GenValidity.DirForest ()
import Data.GenValidity.Mergeful ()
import Data.GenValidity.Mergeful.Item ()
import Data.Mergeful.DirForest

instance GenValid a => GenValid (DirForestSyncRequest a)

instance GenValid a => GenValid (DirForestSyncResponse a)

instance (Ord a, GenValid a) => GenValid (ClientDirForest a)

instance (Ord a, GenValid a) => GenValid (ServerDirForest a)
