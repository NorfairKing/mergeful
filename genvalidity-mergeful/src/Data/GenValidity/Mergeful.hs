{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()

import Data.Mergeful

instance GenUnchecked ClientStore

instance GenValid ClientStore

instance GenUnchecked ServerState

instance GenValid ServerState

instance GenUnchecked ServerStore

instance GenValid ServerStore

instance GenUnchecked SyncRequest

instance GenValid SyncRequest

instance GenUnchecked SyncResponse

instance GenValid SyncResponse

instance GenUnchecked ServerTime

instance GenValid ServerTime
