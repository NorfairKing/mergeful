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
  , singletonDirForest
  , insertDirForest
  , DirForestInsertionError(..)
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
import Data.Validity.Map
import Data.Word
import qualified System.FilePath as FP

import Data.Validity.Path
import Path

import Control.Applicative
import Control.DeepSeq
import Control.Monad

import Data.Mergeful.Item
import Data.Mergeful.Timed

data DirTree a
  = NodeFile a
  | NodeDir (DirForest a)
  deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (DirTree a)

newtype DirForest a =
  DirForest
    { unDirForest :: Map FilePath (DirTree a)
    }
  deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (DirForest a) where
  validate df@(DirForest m) =
    mconcat
      [ genericValidate df
      , decorateMap m $ \p dt ->
          let isTopLevel p = parent p == [reldir|./|]
           in case dt of
                NodeFile _ ->
                  case parseRelFile p of
                    Nothing -> invalid $ "cannot parse as a relative directory: " <> p
                    Just rd ->
                      mconcat
                        [ decorate "The can path can be parsed as a valid relative dir path" $
                          validate rd
                        , declare "There are no separators on this level" $ isTopLevel rd
                        ]
                NodeDir _ ->
                  case parseRelDir p of
                    Nothing -> invalid $ "cannot parse as a relative directory: " <> p
                    Just rd ->
                      mconcat
                        [ decorate "The can path can be parsed as a valid relative dir path" $
                          validate rd
                        , declare "There are no separators on this level" $ isTopLevel rd
                        ]
      ]

emptyDirForest :: DirForest a
emptyDirForest = DirForest M.empty

singletonDirForest :: Ord a => Path Rel File -> a -> DirForest a
singletonDirForest rp a =
  case insertDirForest rp a emptyDirForest of
    Right df -> df
    _ -> error "There can't have been anything in the way in an empty dir forest."

insertDirForest ::
     forall a. Ord a
  => Path Rel File
  -> a
  -> DirForest a
  -> Either (DirForestInsertionError a) (DirForest a)
insertDirForest rp a df = go [reldir|./|] df (FP.splitDirectories $ fromRelFile rp)
  where
    go ::
         Path Rel Dir
      -> DirForest a
      -> [FilePath]
      -> Either (DirForestInsertionError a) (DirForest a)
    go cur (DirForest ts) =
      \case
        [] -> Right df -- Should not happen, but just insert nothing if it does.
        [f] ->
          case M.lookup f ts of
            Nothing -> Right $ DirForest $ M.insert f (NodeFile a) ts
            Just dt ->
              case dt of
                NodeFile contents -> do
                  let rf = cur </> fromJust (parseRelFile f)
                  Left (FileInTheWay rf contents)
                NodeDir df' -> do
                  let rd = cur </> fromJust (parseRelDir f)
                  Left (DirInTheWay rd df')
        (d:ds) ->
          case M.lookup d ts of
            Nothing -> do
              let rf = fromJust $ parseRelFile $ FP.joinPath ds -- Cannot fail if the original filepath is valid
              pure $ DirForest $ M.singleton d $ NodeDir $ singletonDirForest rf a
            Just dt ->
              case dt of
                NodeFile contents -> do
                  let rf = cur </> fromJust (parseRelFile d)
                  Left (FileInTheWay rf contents)
                NodeDir df' -> do
                  let newCur = cur </> fromJust (parseRelDir d)
                  df' <- go newCur df' ds
                  Right $ DirForest $ M.insert d (NodeDir df') ts

data DirForestInsertionError a
  = FileInTheWay (Path Rel File) a
  | DirInTheWay (Path Rel Dir) (DirForest a)
  deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (DirForestInsertionError a)
