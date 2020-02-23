{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Mergeful.DirTree
  ( DirTree
  ) where

import GHC.Generics (Generic)

import Data.Aeson
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Validity
import Data.Validity.Containers ()
import Data.Word

import Data.Validity.Path
import Path

import Control.Applicative
import Control.DeepSeq
import Control.Monad

import Data.Mergeful.Item
import Data.Mergeful.Timed

data DirTree a
  = NodeFile (Path Rel File) a
  | NodeDir (Path Rel Dir) [DirTree a]
  deriving (Show, Eq, Generic)

type DirForest a = [DirTree a]

instance Validity a => Validity (DirTree a)
  -- TODO make sure that there can be no `foo/bar.txt` in the file
  -- TODO make sure that there can be no `foo/bar` in the dir
  --
