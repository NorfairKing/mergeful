{-# LANGUAGE TypeApplications #-}

module Data.Mergeful.DirTreeSpec where

import Data.Int
import Data.Ratio

import Test.Hspec
import Test.Validity

import Data.GenValidity.Mergeful.DirTree ()
import Data.Mergeful.DirTree

spec :: Spec
spec = do
  genValiditySpec @(DirTree (Ratio Int8))
