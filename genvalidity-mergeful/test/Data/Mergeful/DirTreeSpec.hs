{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Mergeful.DirTreeSpec where

import Data.Int
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
  genValiditySpec @(DirTree (Ratio Int8))
  genValiditySpec @(DirForest (Ratio Int8))
  describe "emptyDirForest" $ it "is valid" $ shouldBeValid (emptyDirForest @Int)
  describe "insertDirForest" $ do
    it "works for this example of a file" $
      forAllValid $ \contents ->
        let f = [relfile|foo|]
         in insertDirForest f (contents :: Int) emptyDirForest `shouldBe`
            Just (DirForest (S.fromList [NodeFile f contents]))
    it "works for this example of a file in a dir" $
      forAllValid $ \contents ->
        let f = [relfile|foo/bar|]
         in insertDirForest f (contents :: Int) emptyDirForest `shouldBe`
            Just (DirForest (S.fromList [NodeFile f contents]))
    it "produces valid forests" $ producesValidsOnValids3 (insertDirForest @Int)
