{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful.Timed where

import Data.GenValidity

import Data.Mergeful.Timed

instance GenUnchecked a => GenUnchecked (Timed a)

instance GenValid a => GenValid (Timed a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked ServerTime

instance GenValid ServerTime where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
