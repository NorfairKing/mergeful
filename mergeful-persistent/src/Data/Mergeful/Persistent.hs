{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Mergeful.Persistent where

import Data.Mergeful
import Database.Persist
import Database.Persist.Sql

deriving instance PersistField ServerTime

deriving instance PersistFieldSql ServerTime
