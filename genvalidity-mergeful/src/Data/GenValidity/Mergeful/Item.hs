{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful.Item where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()

import Test.QuickCheck

import Data.Mergeful.Item

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

instance GenUnchecked ServerTime

instance GenValid ServerTime where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

serverItemAt :: GenValid a => ServerTime -> Gen (ServerItem a)
serverItemAt st = oneof [pure $ ServerEmpty st, ServerFull <$> genValid <*> pure st]

reqAt :: GenValid a => ServerTime -> Gen (ItemSyncRequest a)
reqAt st =
  oneof
    [ pure $ ItemSyncRequestKnown st
    , ItemSyncRequestKnownButChanged <$> genValid <*> pure st
    , pure $ ItemSyncRequestDeletedLocally st
    ]
