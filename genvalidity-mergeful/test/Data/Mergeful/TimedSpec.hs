{-# LANGUAGE TypeApplications #-}

module Data.Mergeful.TimedSpec
  ( spec,
  )
where

import Data.GenValidity.Mergeful.Item ()
import Data.Mergeful.Timed
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @ServerTime
  jsonSpec @ServerTime
  genValidSpec @(Timed Int)
  jsonSpec @(Timed Int)
  describe "initialServerTime" $ it "is valid" $ shouldBeValid initialServerTime
