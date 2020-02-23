{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Mergeful.DirTreeSpec where

import Data.Int
import qualified Data.Map as M
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as S
import Path

import Test.Hspec
import Test.Validity

import Data.GenValidity.Mergeful.DirTree ()
import Data.Mergeful.DirTree

spec :: Spec
spec = do
  genValidSpec @(DirTree Int8)
  genValidSpec @(DirForest Int8)
  describe "emptyDirForest" $ it "is valid" $ shouldBeValid (emptyDirForest @Int)
  describe "singletonDirForest" $
    it "produces valid forests" $ producesValidsOnValids2 (singletonDirForest @Int)
  describe "insertDirForest" $ do
    it "works for this example of a file" $
      forAllValid $ \contents ->
        insertDirForest [relfile|foo|] (contents :: Int) emptyDirForest `shouldBe`
        Right (DirForest (M.singleton "foo" (NodeFile contents)))
    it "works for this example of a file in a dir" $
      forAllValid $ \contents ->
        insertDirForest [relfile|foo/bar|] (contents :: Int) emptyDirForest `shouldBe`
        Right
          (DirForest
             (M.singleton "foo" (NodeDir (DirForest (M.singleton "bar" (NodeFile contents))))))
    it "works for this example of two files in the same dir" $
      forAllValid $ \contents1 ->
        forAllValid $ \contents2 -> do
          let dt = singletonDirForest [relfile|foo/bar1|] (contents1 :: Int)
          insertDirForest [relfile|foo/bar2|] contents2 dt `shouldBe`
            Right
              (DirForest
                 (M.singleton
                    "foo"
                    (NodeDir
                       (DirForest
                          (M.fromList [("bar1", NodeFile contents1), ("bar2", NodeFile contents2)])))))
    it "works for if there the exact same file is in the way" $
      forAllValid $ \f ->
        forAllValid $ \contents1 ->
          forAllValid $ \contents2 -> do
            let dt = singletonDirForest f (contents1 :: Int)
            insertDirForest f contents2 dt `shouldBe` Left (FileInTheWay f contents1)
    it "works for this example with a deeper file in the way" $
      forAllValid $ \contents1 ->
        forAllValid $ \contents2 -> do
          let dt = singletonDirForest [relfile|foo|] (contents1 :: Int)
          insertDirForest [relfile|foo/bar|] contents2 dt `shouldBe`
            Left (FileInTheWay [relfile|foo|] contents1)
    it "works for this example with a dir in the way" $
      forAllValid $ \contents1 ->
        forAllValid $ \contents2 -> do
          let dt = singletonDirForest [relfile|foo/bar|] (contents1 :: Int)
          insertDirForest [relfile|foo|] contents2 dt `shouldBe`
            Left (DirInTheWay [reldir|foo|] (DirForest $ M.singleton "bar" (NodeFile contents1)))
    it "produces valid forests" $ producesValidsOnValids3 (insertDirForest @Int)
