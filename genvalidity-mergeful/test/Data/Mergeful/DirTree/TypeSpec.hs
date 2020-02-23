{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Mergeful.DirTree.TypeSpec where

import qualified Data.ByteString as SB
import Data.Int
import qualified Data.Map as M
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as S
import Path
import Path.IO

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity

import Data.GenValidity.ByteString ()
import Data.GenValidity.Mergeful.DirTree.Type ()
import Data.Mergeful.DirTree.Type

spec :: Spec
spec = do
  genValidSpec @(DirTree Int8)
  genValidSpec @(DirForest Int8)
  describe "emptyDirForest" $ it "is valid" $ shouldBeValid (emptyDirForest @Int)
  describe "singletonDirForest" $
    it "produces valid forests" $ producesValidsOnValids2 (singletonDirForest @Int)
  describe "lookupDirForest" $
    it "produces valid values" $ producesValidsOnValids2 (lookupDirForest @Int)
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
    it "works for this example of the same file in two different directories" $
      forAllValid $ \contents1 ->
        forAllValid $ \contents2 -> do
          let df =
                insertDirForest [relfile|b/a|] contents2 $
                singletonDirForest [relfile|a|] (contents1 :: Int)
          df `shouldBe`
            Right
              (DirForest
                 { unDirForest =
                     M.fromList
                       [ ("a", NodeFile contents1)
                       , ( "b"
                         , NodeDir
                             (DirForest {unDirForest = M.fromList [("a", NodeFile contents2)]}))
                       ]
                 })
    it "produces valid forests" $ producesValidsOnValids3 (insertDirForest @Int)
    it "inserts something that can be found again afterward" $
      forAllValid $ \dirForest ->
        forAllValid $ \path ->
          forAllValid $ \contents ->
            case insertDirForest path (contents :: Int) dirForest of
              Left _ -> pure () -- Fine.
              Right dirForest' -> lookupDirForest path dirForest' `shouldBe` Just contents
  describe "dirForestToMap" $ do
    it "works for this example with a file" $
      forAllValid $ \contents ->
        let df = DirForest (M.singleton "foo" (NodeFile (contents :: Int)))
         in dirForestToMap df `shouldBe` M.fromList [([relfile|foo|], contents)]
    it "works for this example with a directory" $
      forAllValid $ \contents ->
        let df =
              DirForest
                (M.singleton
                   "foo"
                   (NodeDir (DirForest (M.singleton "bar" (NodeFile (contents :: Int))))))
         in dirForestToMap df `shouldBe` M.fromList [([relfile|foo/bar|], contents)]
    it "works for this example of two files in the same dir" $
      forAllValid $ \contents1 ->
        forAllValid $ \contents2 -> do
          let df =
                DirForest
                  (M.singleton
                     "foo"
                     (NodeDir
                        (DirForest
                           (M.fromList
                              [("bar1", NodeFile contents1), ("bar2", NodeFile (contents2 :: Int))]))))
          dirForestToMap df `shouldBe`
            M.fromList [([relfile|foo/bar1|], contents1), ([relfile|foo/bar2|], contents2)]
    it "works for this example" $
      forAllValid $ \contents1 ->
        forAllValid $ \contents2 -> do
          let df =
                DirForest
                  (M.singleton
                     "a"
                     (NodeDir
                        (DirForest
                           (M.fromList
                              [("a", NodeFile contents1), ("b", NodeFile (contents2 :: Int))]))))
          dirForestToMap df `shouldBe`
            M.fromList [([relfile|a/a|], contents1), ([relfile|a/b|], contents2)]
    it "produces valid maps" $ producesValidsOnValids (dirForestToMap @Int)
  describe "readDirForest" $ do
    it "reads an empty forest if the directory doesn't exist" $ do
      tdirDeleted <- withSystemTempDir "mergeful-dirtree" pure
      dirForest' <- readDirForest tdirDeleted (SB.readFile . fromAbsFile)
      dirForest' `shouldBe` emptyDirForest
    modifyMaxSuccess (`div` 10) $
      modifyMaxSize (`div` 2) $ do
        it "reads valid forests" $
          forAllValid $ \dirForest ->
            withSystemTempDir "mergeful-dirtree" $ \tdir -> do
              writeDirForest tdir dirForest $ \p contents -> SB.writeFile (fromAbsFile p) contents
              dirForest' <- readDirForest tdir (SB.readFile . fromAbsFile)
              shouldBeValid dirForest'
        it "reads what was written" $
          forAllValid $ \dirForest ->
            withSystemTempDir "mergeful-dirtree" $ \tdir -> do
              writeDirForest tdir dirForest $ \p contents -> SB.writeFile (fromAbsFile p) contents
              dirForest' <- readDirForest tdir (SB.readFile . fromAbsFile)
              dirForest' `shouldBe` dirForest
