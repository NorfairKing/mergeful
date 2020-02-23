{-# LANGUAGE DeriveGeneric #-}

module Data.Mergeful.DirForest
  ( module Data.Mergeful.DirForest
  , ItemMergeStrategy(..)
  ) where

import GHC.Generics (Generic)

import Data.Validity

import Data.Mergeful.DirTree.Type
import Data.Mergeful.Item

newtype ClientForest a =
  ClientForest
    { unClientForest :: DirForest (ClientItem a)
    }
  deriving (Show, Eq, Generic)

instance (Validity a, Ord a) => Validity (ClientForest a)

initialClientForest :: ClientForest a
initialClientForest = ClientForest emptyDirForest

type ServerForest a = DirForest a

initialServerForest :: ServerForest a
initialServerForest = emptyDirForest

-- instance (Validity a, Ord a) => Validity (ServerForest a)
newtype ForestSyncRequest a =
  ForestSyncRequest
    { unForestSyncRequest :: DirForest (ItemSyncRequest a)
    }
  deriving (Show, Eq, Generic)

instance (Validity a, Ord a) => Validity (ForestSyncRequest a)

makeForestSyncRequest :: ClientForest a -> ForestSyncRequest a
makeForestSyncRequest = ForestSyncRequest . fmap makeItemSyncRequest . unClientForest

newtype ForestSyncResponse a =
  ForestSyncResponse
    { unForestSyncResponse :: DirForest (ItemSyncResponse a)
    }
  deriving (Show, Eq, Generic)

instance (Validity a, Ord a) => Validity (ForestSyncResponse a)

mergeForestSyncResponseFromClient :: ClientForest a -> ForestSyncResponse a -> ClientForest a
mergeForestSyncResponseFromClient = undefined

mergeForestSyncResponseFromServer :: ClientForest a -> ForestSyncResponse a -> ClientForest a
mergeForestSyncResponseFromServer =
  mergeForestSyncResponseUsingStrategy
    ItemMergeStrategy
      { itemMergeStrategyMergeChangeConflict = \_ serverItem -> serverItem
      , itemMergeStrategyMergeClientDeletedConflict = \serverItem -> Just serverItem
      , itemMergeStrategyMergeServerDeletedConflict = \_ -> Nothing
      }

mergeForestSyncResponseUsingStrategy ::
     ItemMergeStrategy a -> ClientForest a -> ForestSyncResponse a -> ClientForest a
mergeForestSyncResponseUsingStrategy = undefined

processServerForestSync ::
     ServerForest a -> ForestSyncRequest a -> (ForestSyncResponse a, ServerForest a)
processServerForestSync = undefined
