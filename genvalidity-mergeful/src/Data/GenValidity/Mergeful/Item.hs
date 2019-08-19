{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful.Item where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()

import Test.QuickCheck

import Data.Mergeful.Item

instance GenUnchecked a => GenUnchecked (ClientStore a)

instance GenValid a => GenValid (ClientStore a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (ServerStore a)

instance GenValid a => GenValid (ServerStore a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (SyncRequest a)

instance GenValid a => GenValid (SyncRequest a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (SyncResponse a)

instance GenValid a => GenValid (SyncResponse a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked ServerTime

instance GenValid ServerTime where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

storeAt :: GenValid a => ServerTime -> Gen (ServerStore a)
storeAt st = oneof [pure $ ServerEmpty st, ServerFull <$> genValid <*> pure st]

reqAt :: GenValid a => ServerTime -> Gen (SyncRequest a)
reqAt st =
  oneof
    [ pure $ SyncRequestKnown st
    , SyncRequestKnownButChanged <$> genValid <*> pure st
    , pure $ SyncRequestDeletedLocally st
    ]
