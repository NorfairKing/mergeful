{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Mergeful.DirTree
  ( DirTree(..)
  , DirForest(..)
  , emptyDirForest
  , insertDirForest
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
import System.FilePath as FP

import Data.Validity.Path
import Path

import Control.Applicative
import Control.DeepSeq
import Control.Monad

import Data.Mergeful.Item
import Data.Mergeful.Timed

data DirTree a
  = NodeFile (Path Rel File) a
  | NodeDir (Path Rel Dir) (DirForest a)
  deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (DirTree a) where
  validate dt =
    mconcat
      [ genericValidate dt
      , declare "There are no separators on this level" $
        let isTopLevel p = parent p == [reldir|./|]
         in case dt of
              NodeFile rf _ -> isTopLevel rf
              NodeDir rd _ -> isTopLevel rd
      ]

dirTreeFirstPiece :: DirTree a -> FilePath
dirTreeFirstPiece =
  \case
    NodeFile rf _ -> fromRelFile rf
    NodeDir rd _ -> fromRelDir rd

newtype DirForest a =
  DirForest
    { unDirForest :: Set (DirTree a)
    }
  deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (DirForest a) where
  validate df@(DirForest s) =
    mconcat
      [ genericValidate df
      , declare "There are no conflicting files and directories in this directory forest" $
        distinct $ map dirTreeFirstPiece (S.toList s)
      ]

distinct :: Ord a => [a] -> Bool
distinct ls = ls == S.toList (S.fromList ls)

emptyDirForest :: DirForest a
emptyDirForest = DirForest S.empty
-- insertDirForest ::
--      forall a. Ord a
--   => Path Rel File
--   -> a
--   -> DirForest a
--   -> Maybe (DirForest a)
-- insertDirForest rp a df = go df (splitDirectories $ fromRelFile rp)
--   where
--     go :: DirForest a -> [FilePath] -> Maybe (DirForest a)
--     go (DirForest ts) =
--       \case
--         [] -> pure df -- Should not happen, but just insert nothing if it does.
--         [f] -> do
--           p <- parseRelFile f
--           pure $ DirForest $ S.insert (NodeFile p a) ts
--         (d:ds)  -> do
--           p <- parseRelDir d
--           pure
