{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Mergeful.DirTree.Type
  ( DirTree(..)
  , DirForest(..)
  , emptyDirForest
  , singletonDirForest
  , lookupDirForest
  , insertDirForest
  , DirForestInsertionError(..)
  , dirForestToMap
  , readDirForest
  , writeDirForest
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
import Path.IO

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class

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
                    Just rf ->
                      mconcat
                        [ decorate "The can path can be parsed as a valid relative dir path" $
                          mconcat
                            [ declare "and to the same path, no less" $ fromRelFile rf == p
                            , validate rf
                            ]
                        , declare "There are no separators on this level" $ isTopLevel rf
                        ]
                NodeDir (DirForest df') ->
                  mconcat
                    [ declare "The contained dirforest is nonempty" $ not $ M.null df'
                    , declare "the path has no trailing path separator" $
                      not $ FP.hasTrailingPathSeparator p
                    , case parseRelDir p of
                        Nothing -> invalid $ "cannot parse as a relative directory: " <> p
                        Just rd ->
                          mconcat
                            [ decorate "The can path can be parsed as a valid relative dir path" $
                              mconcat
                                [ declare "and to the same path, no less" $
                                  FP.dropTrailingPathSeparator (fromRelDir rd) == p
                                , validate rd
                                ]
                            , declare "There are no separators on this level" $ isTopLevel rd
                            ]
                    ]
      ]

emptyDirForest :: DirForest a
emptyDirForest = DirForest M.empty

singletonDirForest :: Ord a => Path Rel File -> a -> DirForest a
singletonDirForest rp a =
  case insertDirForest rp a emptyDirForest of
    Right df -> df
    _ -> error "There can't have been anything in the way in an empty dir forest."

lookupDirForest ::
     forall a. Ord a
  => Path Rel File
  -> DirForest a
  -> Maybe a
lookupDirForest rp df = go df (FP.splitDirectories $ fromRelFile rp)
  where
    go :: DirForest a -> [FilePath] -> Maybe a
    go (DirForest ts) =
      \case
        [] -> Nothing
        [f] -> do
          dt <- M.lookup f ts
          case dt of
            NodeFile contents -> Just contents
            _ -> Nothing
        (d:ds) -> do
          dt <- M.lookup d ts
          case dt of
            NodeDir dt -> go dt ds
            _ -> Nothing

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
            Nothing -> pure $ DirForest $ M.insert f (NodeFile a) ts
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
              pure $ DirForest $ M.insert d (NodeDir $ singletonDirForest rf a) ts
            Just dt ->
              case dt of
                NodeFile contents -> do
                  let rf = cur </> fromJust (parseRelFile d)
                  Left (FileInTheWay rf contents)
                NodeDir df' -> do
                  let newCur = cur </> fromJust (parseRelDir d)
                  df'' <- go newCur df' ds
                  pure $ DirForest $ M.insert d (NodeDir df'') ts

data DirForestInsertionError a
  = FileInTheWay (Path Rel File) a
  | DirInTheWay (Path Rel Dir) (DirForest a)
  deriving (Show, Eq, Ord, Generic)

instance (Validity a, Ord a) => Validity (DirForestInsertionError a)

dirForestToMap :: DirForest a -> Map (Path Rel File) a
dirForestToMap = M.foldlWithKey go M.empty . unDirForest
  where
    go :: Map (Path Rel File) a -> FilePath -> DirTree a -> Map (Path Rel File) a
    go m path =
      \case
        NodeFile contents ->
          let rf = fromJust (parseRelFile path) -- Cannot fail if the original dirforest is valid
           in M.insert rf contents m
        NodeDir df ->
          let rd = fromJust (parseRelDir path) -- Cannot fail if the original dirforest is valid
           in M.union m $ M.mapKeys (rd </>) (dirForestToMap df)

readDirForest ::
     forall a b m. (Show a, Ord a, MonadIO m)
  => Path b Dir
  -> (Path b File -> m a)
  -> m (DirForest a)
readDirForest root readFunc = do
  mFiles <- liftIO $ forgivingAbsence $ snd <$> listDirRecurRel root
  foldM go emptyDirForest $ fromMaybe [] mFiles
  where
    go df p = do
      contents <- readFunc $ root </> p
      case insertDirForest p contents df of
        Left e ->
          error
            "There can't have been anything in the way while reading a dirforest, but there was."
        Right df' -> pure df'

writeDirForest ::
     forall a b. Ord a
  => Path b Dir
  -> DirForest a
  -> (Path b File -> a -> IO ())
  -> IO ()
writeDirForest root dirForest writeFunc =
  forM_ (M.toList $ dirForestToMap dirForest) $ \(path, contents) -> do
    let f = root </> path
    ensureDir $ parent f
    writeFunc f contents
