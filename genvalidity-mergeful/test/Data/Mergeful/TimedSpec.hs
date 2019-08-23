{-# LANGUAGE TypeApplications #-}

module Data.Mergeful.TimedSpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

import Data.Mergeful.Timed

import Data.GenValidity.Mergeful.Item ()

spec :: Spec
spec = do
  genValidSpec @ServerTime
  jsonSpecOnValid @ServerTime
  genValidSpec @(Timed Int)
  jsonSpecOnValid @(Timed Int)
  describe "initialServerTime" $ it "is valid" $ shouldBeValid initialServerTime
