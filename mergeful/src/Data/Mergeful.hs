{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A way to synchronise a single item with safe merge conflicts.
--
-- The setup is as follows:
--
-- * A central server is set up to synchronise with
-- * Each client synchronises with the central server, but never with eachother
--
--
-- A central server should operate as follows:
--
-- * The server accepts a 'SyncRequest'.
-- * The server performs operations according to the functionality of 'processServerSync'.
-- * The server respons with a 'SyncResponse'.
--
--
-- A client should operate as follows:
--
-- * The client produces a 'SyncRequest' with 'makeSyncRequest'.
-- * The client sends that request to the central server and gets a 'SyncResponse'.
-- * The client then updates its local store with 'mergeSyncResponseIgnoreProblems'.
module Data.Mergeful
  ( ClientStore(..)
  , SyncRequest(..)
  , SyncResponse(..)
  , makeSyncRequest
  , mergeSyncResponseIgnoreProblems
  , ServerStore(..)
  , processServerSync
  ) where

import GHC.Generics (Generic)

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.Map ()

import Data.Mergeful.Item

data ClientStore i a =
  ClientStore
    { clientStoreAddedItems :: [a]
    , clientStoreSyncedItems :: Map i (a, ServerTime)
    , clientStoreSyncedButChangedItems :: Map i (a, ServerTime)
    , clientStoreDeletedItems :: Map i ServerTime
    }
  deriving (Show, Eq, Generic)

instance (Validity i, Ord i, Validity a) => Validity (ClientStore i a) where
  validate cs@ClientStore {..} =
    mconcat
      [ genericValidate cs
      , declare "There are no duplicate IDs" $
        distinct $
        concat $
        [ M.keys clientStoreSyncedItems
        , M.keys clientStoreSyncedButChangedItems
        , M.keys clientStoreDeletedItems
        ]
      ]

data ServerStore i a =
  ServerStore
    { serverStoreItems :: Map i (a, ServerTime)
    }
  deriving (Show, Eq, Generic)

instance (Validity i, Ord i, Validity a) => Validity (ServerStore i a)

data SyncRequest i a =
  SyncRequest
    { syncRequestNewItems :: [a]
    , syncRequestKnownItems :: Map i ServerTime
    , syncRequestKnownButChangedItems :: Map i (a, ServerTime)
    , syncRequestDeletedItems :: Map i ServerTime
    }
  deriving (Show, Eq, Generic)

instance (Validity i, Ord i, Validity a) => Validity (SyncRequest i a) where
  validate sr@SyncRequest {..} =
    mconcat
      [ genericValidate sr
      , declare "There are no duplicate IDs" $
        distinct $
        concat $
        [ M.keys syncRequestKnownItems
        , M.keys syncRequestKnownButChangedItems
        , M.keys syncRequestDeletedItems
        ]
      ]

data SyncResponse i a =
  SyncResponse
  deriving (Show, Eq, Generic)

instance (Validity i, Validity a) => Validity (SyncResponse i a)

makeSyncRequest :: ClientStore i a -> SyncRequest i a
makeSyncRequest ClientStore {..} =
  SyncRequest
    { syncRequestNewItems = clientStoreAddedItems
    , syncRequestKnownItems = M.map snd clientStoreSyncedItems
    , syncRequestKnownButChangedItems = clientStoreSyncedButChangedItems
    , syncRequestDeletedItems = clientStoreDeletedItems
    }

mergeSyncResponseIgnoreProblems :: ClientStore i a -> SyncResponse i a -> ClientStore i a
mergeSyncResponseIgnoreProblems = undefined

-- | Process a sync request from the server
processServerSync ::
     m i -- ^ The action that is guaranteed to generate unique identifiers
  -> ServerStore i a
  -> SyncRequest i a
  -> m (SyncResponse i a, ServerStore i a)
processServerSync = undefined

distinct :: Eq a => [a] -> Bool
distinct ls = nub ls == ls
